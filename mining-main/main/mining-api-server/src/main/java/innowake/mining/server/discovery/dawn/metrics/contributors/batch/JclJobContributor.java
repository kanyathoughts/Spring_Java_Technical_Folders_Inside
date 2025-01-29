/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;

import com.google.common.collect.Sets;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.FileAccess;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.batch.DiscoveryJclContentProvider;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider.JclParseResult;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.discovery.config.utility.UtilityOutbound;
import innowake.mining.shared.discovery.config.utility.XmlStep;
import innowake.mining.shared.discovery.config.utility.XmlStep.AccessType;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.DefaultExtension;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.jcl.parser.api.Disposition;
import innowake.ndt.jcl.parser.api.IDDModelInternal;
import innowake.ndt.jcl.parser.env.JclError;
import innowake.ndt.jcl.parser.model.JCL;
import innowake.ndt.jcl.parser.model.Step;
import innowake.ndt.jcl.parser.model.StepExec;
import innowake.ndt.jcl.parser.model.StepIf;

/**
 * Dawn contributor for JCL_JOB files.
 */
@Component
public class JclJobContributor extends AbstractJclContributor {
	
	public static final String DFSRRC00 = "DFSRRC00";
	public static final String RESOLVED_FILE_ACCESS_PROGRAMS = "RESOLVED_FILE_ACCESS_PROGRAMS_";
	private static final Logger LOG = LoggerFactory.getLogger(JclJobContributor.class);
	private static final Set<ModuleType> PROGRAM_TYPES = Sets.newHashSet(ModuleType.COBOL_PROGRAM, ModuleType.NATURAL_PROGRAM, ModuleType.PL1_PROGRAM,
			ModuleType.PL1_MAINPROGRAM, ModuleType.EASYTRIEVE_PROGRAM, ModuleType.C_PROGRAM, ModuleType.BASIC_PROGRAM, ModuleType.CSD_PROGRAM,
			ModuleType.ASSEMBLER_PROGRAM, ModuleType.ASSEMBLER_MACRO);
	private static final ModuleType[] PROGRAM_TYPES_ARRAY = PROGRAM_TYPES.toArray(new ModuleType[] {});
	private static final Pattern CNTL_CARD_CONTINUATION_CHAR = Pattern.compile("-\\s*\\n");
	private static final Pattern INLINE_PROC_PATTERN = Pattern.compile("\\..*");
	private static final String INSTREAM_PROPERTY = "*";
	private static final Pattern PATTERN_READ_TABLE = Pattern.compile("SELECT|FROM");
	private static final Pattern PATTERN_STORE_TABLE = Pattern.compile("INTO|INSERT");
	private static final Pattern PATTERN_UPDATE_TABLE = Pattern.compile("UPDATE");
	private static final Pattern PATTERN_DELETE_TABLE = Pattern.compile("DELETE");
	private static final Pattern CONTROL_CARD_TRAILING_NUMBERS = Pattern.compile("(?<=^.{72})\\d+(?=\\R|$)", Pattern.MULTILINE);
	private static final String VARIABLE_REFERENCE_ERROR = "Variable value is just another reference to itself";
	private static final String DEFAULT_ERROR_MESSAGE = "No error details were provided.";
	private static final String MISSING_PROC_ERROR = "Unable to locate procedure file or possible instream procedure not properly terminated with 'PEND'";
	private static final String DUE_TO_MISSING_FILE = "DUE TO MISSING FILE:";
	private static final Set<String> WARNING_MESSAGES = Set.of(VARIABLE_REFERENCE_ERROR);

	private final SourceCachingService sourceService;
	private final List<JclUtilityContributor> jclUtilityContributors;
	
	private boolean isDFSRRC00UtilityAvailable;

	public JclJobContributor(final SourceCachingService sourceService, final List<JclUtilityContributor> jclUtilityContributors) {
		this.sourceService = sourceService;
		this.jclUtilityContributors = jclUtilityContributors;
	}

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.JCL && sourceObject.getType() == Type.JOB;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.JCL_JOB);

		if (sourceObject.getContent().toString().isBlank()) {
			calculateSourceMetrics(rootModule, sourceObject, 0);
			rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
			return;
		}
		
		final SourceMetrics sourceMetrics = calculateSourceMetrics(rootModule, sourceObject);
		sourceMetrics.setComplexityMcCabe(0);

		List<JCL> batchParseResult = null;
		JclParseResult jclParseResult = null;
		try {
			final var jclContentProvider = new DiscoveryJclContentProvider(context.getProjectId(), sourceService, context.getSearchOrders(),
					sourceObject.getPath());
			jclParseResult = getParseResult(sourceObject, context, jclContentProvider);
			batchParseResult = jclParseResult.getFull();
			final String assembledJcl = jclParseResult.getJclContents();
			final JobControl jobControl = new JobControl(sourceService, parserProviderService, builder, context);
			final String jclContents = sourceObject.getContent().toString();
			final var dawnBatchStatementCollector = new DawnBatchStatementCollector(batchParseResult, builder, rootModule);
			/*
			 * This complexity calculation can be removed once the GenericMetricsContributor
			 * is supported for JCL complexity calculation
			 */
			final int ifStepsCount = (int) batchParseResult.stream().map(JCL::getSteps).map(JobControl::getAllStepIf).mapToLong(List::size).sum();
			final int complexity = ifStepsCount + 1;
			sourceMetrics.setComplexityMcCabe(complexity);
			rootModule.addAdditionalInfo(sourceMetrics);

			final Set<String> inlineProcs = new HashSet<>();
			/*
			 * Collect inline procs for Job alone as the inline procs could anyway be
			 * collected from the assembled proc in job
			 */
			jobControl.collectInlineProcs(sourceObject, jclContents, assembledJcl, inlineProcs);
			jobControl.collectIncludeGroups(assembledJcl, rootModule);

			/* Collects Statements for Job */
			final Set<String> jobStatements = new HashSet<>();
			dawnBatchStatementCollector.collectStatementsFromJob(jobStatements);

			collectJobLibDatasets(batchParseResult, builder, rootModule);
			collectStepLibDatasets(batchParseResult, builder, context);

			/* parse result is empty if module contains only comments */
			if ( ! batchParseResult.isEmpty()) {
				isDFSRRC00UtilityAvailable  = context.getConfig().getUtilityList().isUtility(DFSRRC00);
				final List<Step> steps = batchParseResult.stream().flatMap(JobControl::getJCLStepsRecursively)
						.filter(step -> step instanceof StepExec || step instanceof StepIf).collect(Collectors.toList());

				/* The duplicate steps (steps inside inlineProc) creation is avoided
				 * by checking if the step is inside inlineProc and skip it and create this 
				 * proc step only once while parsing the proc, but there is challenge when the proc defined within a job, for such procs
				 * we can't uniquely identified it's steps as we don't parse the procs within the job separately, hence let's declare 
				 * this new set which is used to capture the unique steps */
				final Set<String> stepsWithinSameSource = new HashSet<>();

				final Set<String> stepStatements = new HashSet<>();
				final Set<String> stepErrors = new HashSet<>();
				/* Collect all the dependencies from the steps */
				steps.forEach(step -> collectSteps(jobControl, step, builder, rootModule, context, dawnBatchStatementCollector, sourceObject, inlineProcs,
						stepsWithinSameSource, stepStatements, stepErrors, jclContentProvider.getResolvedProcs()));
				jobControl.collectEasytrieve(steps, rootModule);
			}
		} catch (final DiscoveryException e) {
			rootModule.addAdditionalInfo(sourceMetrics);
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
		} catch (final WorkerCancellationException e) {
			rootModule.addAdditionalInfo(sourceMetrics);
			LOG.error("Parser timeout error occurred while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_TIMEOUT, e.getMessage());
			/* unable to parse so nothing more to discover */
		} catch (final Exception e) {
			rootModule.addAdditionalInfo(sourceMetrics);
			LOG.error("Exception occurred while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		} catch (final Throwable e) {
			LOG.error("Unexpected error occurred while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}

		/* collect errors in non-strict mode */
		if (jclParseResult != null && batchParseResult != null) {
			final var utilityList = context.getConfig().getUtilityList();
			collectErrors(rootModule, jclParseResult, batchParseResult, utilityList);
		}

	}

	private void collectSteps(final JobControl jobControl, final Step step, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final DiscoveryContext context,
			final DawnBatchStatementCollector dawnBatchStatementCollector, final SourcePojo sourceObject, final Set<String> inlineProcs,
			final Set<String> stepsWithinSameSource, final Set<String> stepStatements, final Set<String> stepErrors, final Map<String, String> resolvedProcs) {
		if (step instanceof StepExec) {
			final ModuleBuilder stepSubModule = createPhysicalToStepBinding((StepExec) step, builder, rootModule, sourceObject, stepsWithinSameSource,
					resolvedProcs);
			final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer = getConditionalDependencyConsumer((StepExec) step, sourceObject);

			final JclContributorContext jclContext = new JclContributorContext(builder, rootModule, context, jobControl, Optional.of(conditionalDependencyConsumer));
			externalStepContributors(jclContext, (StepExec) step, stepSubModule);
			/* Collects statements from steps*/
			dawnBatchStatementCollector.collectStatementsFromSteps((StepExec) step, stepSubModule, stepStatements, resolvedProcs);

			createStepToPhysicalBinding(context, builder, (StepExec) step, stepSubModule, inlineProcs, conditionalDependencyConsumer);
			createStepToDDBinding(context, (StepExec) step, builder, stepSubModule, stepErrors, conditionalDependencyConsumer);
			createStepToControlCardBinding(step, stepSubModule, context, conditionalDependencyConsumer);
			createStepToSqlBinding(jobControl, context, step, stepSubModule, conditionalDependencyConsumer);
			createStepToEZTBinding(step, stepSubModule, conditionalDependencyConsumer);

		} else if (step instanceof StepIf) {
			JobControl.getIfStepChildSteps((StepIf) step).forEach(childStep -> collectSteps(jobControl, childStep, builder, rootModule, context,
					dawnBatchStatementCollector, sourceObject, inlineProcs, stepsWithinSameSource, stepStatements, stepErrors, resolvedProcs));
		}
	}

	private void externalStepContributors(final JclContributorContext context, final StepExec step, final ModuleBuilder stepSubModule) {
		jclUtilityContributors.stream()
			.filter(contributor -> contributor.accept(step))
			.forEach(contributor -> contributor.contribute(context, step, stepSubModule));
	}

	private static Consumer<DiscoveryBuilder.DependencyBuilder> getConditionalDependencyConsumer(final StepExec step, final SourcePojo sourceObject) {
		return dependencyBuilder -> {
			if (step.getParentStepExec() == null) {
				return;
			}
			final String stepExecCallingProc = step.getParentStepExec().getQualifiedStepName();
			final var conditionalFilter = new ModuleFilter().setNames(stepExecCallingProc).setTypes(ModuleType.JCL_EXEC);
			if (step.getParentStepExec().getParentStepExec() == null) {
				/* The parent step is present inside a job, therefore set the contained module path */
				conditionalFilter.setContainedIn(new ModuleFilter().setPaths(sourceObject.getPath()).setTypes(ModuleType.JCL_JOB));
			}
			dependencyBuilder.setReachedFromModules(conditionalFilter);
		};
	}

	private void createStepToEZTBinding(final Step step, final ModuleBuilder stepSubModule,
			final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer) {
		/* collect external easytrieve program dependencies */
		final String loaderProgram = step.getProperties().get(JobControl.PROGRAM);
		final List<IDDModelInternal> modelInternal = step.getDDs().get(JobControl.SYSIN);
		if (modelInternal != null && JobControl.EZT_LOADER.equals(loaderProgram)) {
			modelInternal.stream().map(dd -> dd.getProperties().getOrDefault(JobControl.DSN, dd.getProperties().get(JobControl.DSNAME)))
					.filter(Objects::nonNull).map(JobControl::getNameInParentheses).filter(Optional::isPresent)
					.forEach(eztName -> {
						final var dependency = stepSubModule.declareDependency(RelationshipType.CALLS,
								new ModuleFilter().setNames(eztName.get()).setTypes(ModuleType.EASYTRIEVE_PROGRAM), ResolutionFlag.MERGE_DUPLICATES)
								.setBinding(Binding.EARLY);
						conditionalDependencyConsumer.accept(dependency);
					});
		}
	}

	private ModuleBuilder createPhysicalToStepBinding(final StepExec step, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule,
			final SourcePojo sourceObject, final Set<String> stepsWithinSameSource, final Map<String, String> resolvedProcs) {
		final ModuleBuilder stepSubModule;
		final String qualifiedStepName = step.getQualifiedStepName();

		if ( ! stepsWithinSameSource.add(qualifiedStepName)) {
			final ModuleFilter parentFilter = step.isInsideInlinedProc() ? getProcFilter(resolvedProcs, qualifiedStepName, ModuleType.JCL_PROC)
					: new ModuleFilter().setNames(sourceObject.getName())
							.setTypes(ModuleType.JCL_JOB).setPaths(sourceObject.getPath());
			return builder.anchorTo(new ModuleFilter().setNames(qualifiedStepName).setTypes(JobControl.getExecType(step)).setContainedIn(parentFilter));
		}

		/* To handle the instream proc steps */
		if (step.isInsideInstreamProc()) {
			final String parentQualifiedStepName = step.getParentStepExec().getQualifiedStepName();
			final String parentStepName = parentQualifiedStepName.substring(0, parentQualifiedStepName.indexOf('.') + 1)
					+ qualifiedStepName.substring(0, qualifiedStepName.indexOf('.'));
			stepSubModule = builder.declareSubModule(qualifiedStepName, JobControl.getExecType(step));
			final ModuleFilter procFilter = getProcFilter(resolvedProcs, parentQualifiedStepName, ModuleType.JCL_JOB);
			builder.anchorTo(new ModuleFilter().setNames(parentStepName).setTypes(ModuleType.JCL_INLINE_PROC).setContainedIn(procFilter))
				.declareDependency(RelationshipType.CALLS, stepSubModule).setBinding(Binding.LATE);
			return stepSubModule;
		}

		if (step.isInsideInlinedProc()) {
			final ModuleFilter procFilter = getProcFilter(resolvedProcs, qualifiedStepName, ModuleType.JCL_PROC);
			stepSubModule = builder.anchorTo(new ModuleFilter().setNames(qualifiedStepName).setTypes(JobControl.getExecType(step)).setContainedIn(procFilter));
			builder.anchorTo(procFilter)
					.declareDependency(RelationshipType.CALLS, stepSubModule).setBinding(Binding.LATE);
		} else {
			stepSubModule = builder.declareSubModule(qualifiedStepName, JobControl.getExecType(step));
			rootModule.declareDependency(RelationshipType.CALLS, stepSubModule).setBinding(Binding.LATE);
		}
		return stepSubModule;
	}

	private ModuleFilter getProcFilter(final Map<String, String> resolvedProcs, final String qualifiedStepName, final ModuleType moduleType) {
		final var procName = qualifiedStepName.substring(0, qualifiedStepName.indexOf('.'));
		final var resolvedProcPath = resolvedProcs.get(procName);
		final ModuleFilter procFilter = new ModuleFilter().setNames(procName).setTypes(moduleType);
		if (resolvedProcPath != null) {
			procFilter.setPaths(resolvedProcPath);
		}
		return procFilter;
	}

	private void createStepToControlCardBinding(final Step step, final ModuleBuilder stepSubModule, final DiscoveryContext context,
			final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer) {
		final String pgmName = step.getProperties().get(JobControl.PROGRAM);

		if (pgmName == null) {
			return;
		}
		final Config config = context.getConfig();
		for (final String stepName : config.getPgmStepNameList()) {
			/*
			 * When the step uses easytrieve program loader, the file specified in the SYSIN
			 * is already categorized as Easytrieve program. Hence we should not create a
			 * dependency as control card.
			 */
			if ( ! pgmName.equals(JobControl.EZT_LOADER) || ! stepName.equalsIgnoreCase("SYSIN")) {
				final List<IDDModelInternal> dds = Optional.ofNullable(step.getDDs().get(stepName)).orElse(Collections.emptyList());
				for (final IDDModelInternal ddModel : dds) {
					final String controlCardName = JobControl.getControlCardName(ddModel);
					if (controlCardName.isEmpty()) {
						continue;
					}

					final Map<String, String> properties = new HashMap<>(ddModel.getProperties());
					properties.put(JobControl.ID_NAME, ddModel.getId().trim());
					final Set<Map<String, String>> propertiesSet = new HashSet<>();
					propertiesSet.add(properties);

					final String dsn = properties.getOrDefault(JobControl.DSN, properties.get(JobControl.DSNAME));
					final Optional<String> controlCardSearchPath = Optional.ofNullable(dsn).map(dsname -> {
						final int memberNameIndex = dsname.indexOf('(');
						final String baseName = memberNameIndex >= 0 ? dsname.substring(0, memberNameIndex) : dsname;
						return "**/" + baseName.replace('.', '/') + "/**/*." + DefaultExtension.CRD.toString().toLowerCase();
					});

					final var moduleFilter = new ModuleFilter();
					moduleFilter.setNames(controlCardName);
					moduleFilter.setTypes(ModuleType.JCL_CONTROLCARD);
					final List<ModuleFilter> moduleFilterList = new ArrayList<>();
					moduleFilterList.add(moduleFilter);
					controlCardSearchPath.ifPresent(controlCardPath -> moduleFilterList.add(new ModuleFilter().setPathPatterns(controlCardPath)));

					final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>();
					attributeMap.put(ModelAttributeKey.PROPERTIES, propertiesSet);
					final var dependency =  stepSubModule.declareDependency(RelationshipType.CALLS, moduleFilterList,
									ResolutionFlag.MERGE_DUPLICATES).setAttributes(attributeMap).setBinding(Binding.LATE);
					conditionalDependencyConsumer.accept(dependency);
				}
			}
		}
	}

	private void createStepToDDBinding(final DiscoveryContext discoveryContext, final StepExec step, final DiscoveryBuilderFromSource builder,
			final ModuleBuilder stepSubModule, final Set<String> stepErrors,final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer) {
		final var config = discoveryContext.getConfig();

		step.getDDs().entrySet().stream().flatMap(ddAssignmentEntry -> ddAssignmentEntry.getValue().stream())
				.filter(JobControl.filterDDsInIgnoreDependencyList(config)).filter(ddModel -> {
					final Set<String> keys = ddModel.getProperties().keySet();
					return keys.contains(JobControl.DSN) || keys.contains(JobControl.DSNAME);
				}).forEach(ddModel -> {
					final String ddName = ddModel.getProperties().getOrDefault(JobControl.DSN, ddModel.getProperties().get(JobControl.DSNAME));
					final String dsnName = cleanUpDDName(ddName);
					if ((StringUtils.isBlank(dsnName) || dsnName.indexOf('&') != -1) || JobControl.isPsFileControlCard(config, ddModel, ddName)) {
						if (dsnName.indexOf('&') != -1 && dsnName.indexOf("&&") == -1) {
							final String message = step.getFullyQualifiedId() + " step has invalid resource file name, contains '&' in " + dsnName;
							if (stepErrors.add(message)) {
								stepSubModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, message);
							}
						}
						return;
					}

					final var moduleFilter = new ModuleFilter();
					moduleFilter.setNames(dsnName);
					moduleFilter.setTypes(ModuleType.RESOURCE_FILE, ModuleType.RESOURCE_VSAM_FILE, ModuleType.RESOURCE_LIB, ModuleType.RESOURCE_GDG_FILE);
					final Map<String, String> properties = new LinkedHashMap<>(ddModel.getProperties());
					properties.remove(JobControl.DSN);
					properties.remove(JobControl.DSNAME);
					properties.put(JobControl.ID_NAME, ddModel.getId());
					final Set<Map<String, String>> propertiesSet = new HashSet<>();
					propertiesSet.add(properties);
					final List<FileAccess> buildDependencyAttributes = buildDependencyAttributes(step, config, ddModel);
					final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>();
					attributeMap.put(ModelAttributeKey.PROPERTIES, propertiesSet);
					attributeMap.put(ModelAttributeKey.FILE_ACCESS_TYPE, buildDependencyAttributes);
					final var valueInsideParenthesis = JobControl.getNameInParentheses(ddName).orElse(StringUtils.EMPTY);
					final var moduleType = JobControl.GDG_FILE_PATTERN.matcher(valueInsideParenthesis).find() ? ModuleType.RESOURCE_GDG_FILE
							: ModuleType.RESOURCE_FILE;
					builder.declareExternalModule(dsnName, moduleType);
					final var dependency = stepSubModule.declareDependency(RelationshipType.ACCESSES, moduleFilter,
									ResolutionFlag.MERGE_DUPLICATES).setAttributes(attributeMap).setBinding(Binding.LATE);

					conditionalDependencyConsumer.accept(dependency);
				});
	}

	

	private List<FileAccess> buildDependencyAttributes(final Step step, final Config config, final IDDModelInternal ddModel) {
		final List<FileAccess> accessTypes = new ArrayList<>();
		final String programName = step.getProperties().get(JobControl.PROGRAM);
		final Optional<UtilityEntity> utility = programName != null ? config.getUtilityList().findUtility(programName) : Optional.empty();
		final List<AccessType> accessTypeList = utility.isPresent()
				? utility.get().getSteps().stream().filter(xmlStep -> xmlStep.getName().equals(ddModel.getId()) && xmlStep.getAccessType() != null)
						.map(XmlStep::getAccessType).collect(Collectors.toList())
				: Collections.emptyList();

		if ( ! accessTypeList.isEmpty()) {
			if (accessTypeList.size() > 1) {
				LOG.warn(String.format("Multiple steps found with the same name %s for utility %s. Please fix the utilities.xml.", ddModel.getId(),
						programName));
			}
			final FileAccess fileAccess = accessTypeList.get(0) == AccessType.WRITE ? FileAccess.WRITE : FileAccess.READ;
			accessTypes.add(fileAccess);
		} else {
			/* Adds the Disposition into the ModelAttributeMap */
			final var disposition = ddModel.getDISP();
			if (disposition != Disposition.UNKNOWN) {
				if (disposition == Disposition.NEW || disposition == Disposition.OLD || disposition == Disposition.DEFAULT) {
					accessTypes.add(ModelAttributeValue.FileAccess.WRITE);
				} else if (disposition == Disposition.SHR) {
					accessTypes.add(ModelAttributeValue.FileAccess.READ);
				} else if (disposition == Disposition.MOD) {
					accessTypes.add(ModelAttributeValue.FileAccess.WRITE);
					accessTypes.add(ModelAttributeValue.FileAccess.READ);
				}
			}
		}
		return accessTypes;
	}

	private void collectJobLibDatasets(final List<JCL> batchParseResult, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		/*
		 * Collecting all the DDs for the JOBLIB and adding modules as
		 * RESOURCE_LIB
		 */
		final List<IDDModelInternal> allDDs = Optional.ofNullable(batchParseResult.iterator().next().getDDs().get(JobControl.JOBLIB))
				.orElse(Collections.emptyList());
		allDDs.stream()
		.map(IDDModelInternal::getDSN)
		.filter(StringUtils::isNotBlank)
		.forEach(dsn -> {
			final ModuleBuilder jobResourceLib = builder.declareExternalModule(dsn, ModuleType.RESOURCE_LIB);
			rootModule.declareDependency(RelationshipType.ACCESSES, jobResourceLib).setBinding(Binding.LATE);
		});
	}

	private void collectStepLibDatasets(final List<JCL> batchParseResult, final DiscoveryBuilderFromSource builder, final DiscoveryContext context) {
		/*
		 * Collecting all the DDs for the STEPLIB excluding DD_IGNORE_DEPENDENCY as
		 * RESOURCE_LIB
		 */
		final Config config = context.getConfig();
		final Set<String> allDsns = JobControl.getAllDDs(batchParseResult.get(0).getAllSteps()).stream()
				.flatMap(Collection::stream)
				.filter(dd -> dd.getId().equals(JobControl.STEPLIB))
				.filter(JobControl.filterDDsInIgnoreDependencyList(config))
				.map(IDDModelInternal::getDSN)
				.filter(StringUtils::isNotBlank)
				.collect(Collectors.toSet());
		/* There are some DDs which are missing while running metrics through incremental scan and those will be captured by below code.*/
		final List<IDDModelInternal> missingDDs = Optional.ofNullable(batchParseResult.iterator().next().getDDs().get(JobControl.STEPLIB))
				.orElse(Collections.emptyList());
		
		final Set<String> missingDsns = missingDDs.stream()
		.filter(JobControl.filterDDsInIgnoreDependencyList(config))
		.map(IDDModelInternal::getDSN)
		.filter(StringUtils::isNotBlank)
		.collect(Collectors.toSet());
		
		allDsns.addAll(missingDsns);
		
		allDsns.forEach(dsn -> builder.declareExternalModule(dsn, ModuleType.RESOURCE_LIB));
	}

	/**
	 * Used to harmonize the DD name provided by batchclipse and full parser. <br>
	 * Example
	 * <li>Provided by batchclipse -> {@code APPL.ODDC.PROD.DB2.CONTROL}
	 * <li>Provided by full parser -> {@code APPL.ODDC.PROD.DB2.CONTROL(MGOIN)}
	 *
	 * @param ddName The DD name to harmonize.
	 * @return The DD name from start till the first brace Example
	 *         {@code APPL.ODDC.PROD.DB2.CONTROL}
	 */
	private static String cleanUpDDName(final String ddName) {
		final int indexOfBrace = ddName.indexOf('(');
		return indexOfBrace == -1 ? ddName : ddName.substring(0, indexOfBrace);
	}

	private void createStepToSqlBinding(final JobControl jobControl, final DiscoveryContext discoveryContext, final Step step, final ModuleBuilder stepSubModule,
			final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer) {
		jobControl.collectTablesInStep(discoveryContext.getConfig(), discoveryContext.getProjectId(), step, stepSubModule).forEach((table, accessTypes) -> {
			final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>();
			attributeMap.put(ModelAttributeKey.DB_ACCESS_TYPE, accessTypes);
			final var dependency = DawnMetricsUtility.declareDependencyToSqlTableWithSchema(stepSubModule, table, ResolutionFlag.MERGE_DUPLICATES);
			dependency.setAttributes(attributeMap);
			conditionalDependencyConsumer.accept(dependency);
		});
	}

	private void createStepToPhysicalBinding(final DiscoveryContext context, final DiscoveryBuilderFromSource builder, final StepExec step,
			final ModuleBuilder stepSubModule, final Set<String> inlineProcs, final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer) {
		final var config = context.getConfig();
		final var projectId = context.getProjectId();
		final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
		final Map<String, String> stepProperties = step.getProperties();

		final String programName = stepProperties.get(JobControl.PROGRAM);

		if (programName != null) {
			final Optional<UtilityEntity> utility = config.getUtilityList().findUtility(programName);
			if (utility.isPresent() && utility.get().isInterface()) {
				utility.get().getParameters().forEach(parameter -> {
					int inboundNumber = 0;
					int outboundNumber = 0;
					final List<String> targets = new ArrayList<>();
					final List<String> imsTargets = new ArrayList<>();
					final var inboundRegex = new LazyPatternSupplier(
							() -> parameter.getInbound().isEmpty() ? Optional.empty() : Optional.of(parameter.getInbound()));
					final var outboundRegex = new LazyPatternSupplier(
							() -> parameter.getOutbound().getOutBoundValue().isEmpty() ? Optional.empty()
									: Optional.of(parameter.getOutbound().getOutBoundValue()));
					final Optional<UtilityOutbound> outbound = Optional.ofNullable(parameter.getOutbound());

					/* collect parm property if any */
					final String parameterProperty = stepProperties.get(JobControl.PARAMETER);
					String parmRawString = null;
					if (parameterProperty != null) {
						parmRawString = JobControl.assembleKeywordAndValue(JobControl.PARAMETER, parameterProperty.toUpperCase());
						if (inboundRegex.isPresent()) {
							inboundNumber += JobControl.countMatch(inboundRegex.get(), parmRawString);
						}

						if (DFSRRC00.equalsIgnoreCase(programName)) {
							outboundNumber += 2;
							final var batchIMSContributor = new BatchIMSContributor(parserProviderService, builder, context, stepSubModule);
							batchIMSContributor.collectMetrics(parmRawString);
							/*
							 * We don't add ims targets directly targets as it's modules are already created
							 * in BatchIMSContributor and need to be created below with targets
							 */
							imsTargets.addAll(batchIMSContributor.getMatchedTargets());
						} else if (outboundRegex.isPresent()) {
							outboundNumber += JobControl.countMatch(outboundRegex.get(), parmRawString);
							targets.addAll(JobControl.searchTargets(outboundRegex.get(), parmRawString));
						}
					}

					for (final String ddName : config.getUtilityParmStepNameList(programName)) {
						final List<IDDModelInternal> dds = Optional.ofNullable(step.getDDs().get(ddName)).orElse(Collections.emptyList());
						for (final IDDModelInternal dd : dds) {
							final String controlCardName = JobControl.getControlCardName(dd);
							if ( ! controlCardName.isEmpty()) {
								final String externalControlCardContent = sourceService.find(q -> q.ofProject(projectId)
																				 		.withName(controlCardName)
																						.withTechnology(Technology.JCL)
																						.withType(Type.CONTROLCARD))
																						.stream()
																						.map(s -> s.getContent().toString()).collect(Collectors.joining())
																						.toUpperCase();
								if (inboundRegex.isPresent()) {
									inboundNumber += JobControl.countMatch(inboundRegex.get(), externalControlCardContent);
								}

								if (outboundRegex.isPresent()) {
									final List<String> targetsMatchingCC = getTargets(outboundRegex, externalControlCardContent, outbound);
									if (CollectionUtils.exists(targetsMatchingCC, str -> StringUtils.isNotBlank((String) str))
											&& ! targets.containsAll(targetsMatchingCC)) {
										targets.addAll(targetsMatchingCC);
										outboundNumber += JobControl.countMatch(outboundRegex.get(), externalControlCardContent);
									}
								}
							}

							/* handle inline parameters and parameters in the DD keywords */
							final List<String> keywords = config.getUtilityParmDDKeywordList();
							for (final String keyword : keywords) {
								final String keywordValue = dd.getProperties().get(keyword);
								if (keywordValue != null && ! keywordValue.trim().isEmpty()) {
									final String inlineProgramContent = keywordValue.toUpperCase();
									if (inboundRegex.isPresent()) {
										inboundNumber += JobControl.countMatch(inboundRegex.get(), inlineProgramContent);
									}
									if (outboundRegex.isPresent()) {
										final List<String> inlineTargets = getTargets(outboundRegex, inlineProgramContent, outbound);
										if (CollectionUtils.exists(inlineTargets, str -> StringUtils.isNotBlank((String) str))
												&& ! targets.containsAll(inlineTargets)) {
											targets.addAll(inlineTargets);
											outboundNumber += JobControl.countMatch(outboundRegex.get(), inlineProgramContent);
										}
									}
								}
							}
						}
					}

					targets.forEach(targetModuleName -> {
						if (StringUtils.isNotBlank(targetModuleName) && outbound.isPresent()) {
							final var technology = outbound.get().getTechnology();
							final var type = outbound.get().getType();
							final var moduleType = ModuleType.fromTechnologyAndType(technology, type);
							/*
							 * If it's SQL we just declare the dependency with orCreateIfMissing and DAWN
							 * will create the module and add the dependency
							 */
							if (technology.equals(Technology.SQL) || type.equals(Type.TABLE)) {
								final var dependency = DawnMetricsUtility.declareDependencyToSqlTableWithSchema(stepSubModule,
												targetModuleName, ResolutionFlag.MERGE_DUPLICATES)
										.setAttributes(getDbAccessType(outboundRegex.get().pattern()));
								conditionalDependencyConsumer.accept(dependency);
							} else if ( ! (technology.equals(Technology.UNKNOWN) || type.equals(Type.UNKNOWN))) {
								final RelationshipType relationshipType = technology.equals(Technology.RESOURCE) && type.equals(Type.VSAM_FILE) ?
										RelationshipType.ACCESSES : RelationshipType.CALLS;
									builder.declareExternalModule(targetModuleName,moduleType);
									final var dependency = stepSubModule.declareDependency(relationshipType, new ModuleFilter().
															setNames(targetModuleName).setTypes(moduleType), ResolutionFlag.MERGE_DUPLICATES)
															.setBinding(Binding.LATE);
									conditionalDependencyConsumer.accept(dependency);
							} else {
								final var dependency = stepSubModule.declareDependency(RelationshipType.CALLS,
										new ModuleFilter().setNames(targetModuleName).setTypes(PROGRAM_TYPES_ARRAY), ResolutionFlag.MERGE_DUPLICATES)
										.setBinding(Binding.LATE);
								conditionalDependencyConsumer.accept(dependency);
							}
						}
					});

					attributes.put(ModelAttributeKey.INBOUND, Integer.valueOf(inboundNumber));
					attributes.put(ModelAttributeKey.OUTBOUND, Integer.valueOf(outboundNumber));
					targets.addAll(imsTargets);
					attributes.put(ModelAttributeKey.OUTBOUND_TARGETS, targets);
				});

			}

		}

		this.createBindingFromStepToProcOrPrg(context, builder, stepSubModule, step, attributes, inlineProcs, conditionalDependencyConsumer);
	}

	private void createBindingFromStepToProcOrPrg(final DiscoveryContext context, final DiscoveryBuilderFromSource builder, final ModuleBuilder source,
			final StepExec step, final ModelAttributeMap<Object> attributes, final Set<String> inlineProcs,
			final Consumer<DiscoveryBuilder.DependencyBuilder> conditionalDependencyConsumer) {
		final Optional<String> stepTargetName = JobControl.getStepExecTargetName(step);
		if (stepTargetName.isEmpty()) {
			return;
		}

		if (step.isExecProc()) {
			final String parentName = INLINE_PROC_PATTERN.matcher(step.getQualifiedStepName()).replaceFirst("");
			final String inlineProcName = parentName + "." + stepTargetName.get();
			if (inlineProcs.contains(inlineProcName)) {
				source.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames(inlineProcName).setTypes(ModuleType.JCL_INLINE_PROC))
						.setAttributes(attributes).setBinding(Binding.LATE);
			} else {
				source.declareDependency(RelationshipType.CALLS,
						new ModuleFilter().setNames(stepTargetName.get()).setTypes(ModuleType.JCL_PROC), ResolutionFlag.MERGE_DUPLICATES)
						.setAttributes(attributes).setBinding(Binding.LATE);
			}

			declareNaturaldependency(context, builder, source, step, attributes);
		} else {
			boolean isNaturalProgram = declareNaturaldependency(context, builder, source, step, attributes);
			if ( ! isNaturalProgram) {
				final String targetName = stepTargetName.get();
				final DependencyBuilder dependency;
				if (isDFSRRC00UtilityAvailable && DFSRRC00.equals(targetName)) {
					final ModuleBuilder utilityModule = builder.declareExternalModule(targetName, ModuleType.UNKNOWN_UTILITY, Origin.ENVIRONMENT);
					dependency = source.declareDependency(RelationshipType.CALLS, utilityModule);
				} else {
					dependency = source.declareDependency(RelationshipType.CALLS,
							new ModuleFilter().setNames(stepTargetName.get()).setTypes(PROGRAM_TYPES), ResolutionFlag.MERGE_DUPLICATES);
				}
				dependency.setAttributes(attributes).setBinding(Binding.LATE);
				conditionalDependencyConsumer.accept(dependency);
			}
		}
	}

	private boolean declareNaturaldependency(final DiscoveryContext context, final DiscoveryBuilder builder, final ModuleBuilder source, final StepExec step,
			final ModelAttributeMap<Object> attributes) {
		final List<IDDModelInternal> allDdsFromCurrentStep = step.getDDs().values().stream().flatMap(List<IDDModelInternal>::stream)
				.collect(Collectors.toList());
		if ( ! allDdsFromCurrentStep.isEmpty())
			for (final IDDModelInternal dd : allDdsFromCurrentStep) {
				final String instreamProperty = dd.getProperty(INSTREAM_PROPERTY);
				final var batchNaturalContributor = new BatchNaturalContributor(parserProviderService, builder, context, source, attributes);
				batchNaturalContributor.collectMetrics(instreamProperty);
				if (batchNaturalContributor.isANaturalProgram()) {
					return true;
				}
			}
		return false;
	}

	public static class LazyPatternSupplier implements Supplier<Pattern> {

		private final Supplier<Optional<String>> supplier;
		Optional<Pattern> pattern = Optional.empty();

		public LazyPatternSupplier(final Supplier<Optional<String>> supplier) {
			this.supplier = supplier;
		}

		private boolean isPresent() {
			if ( ! pattern.isPresent()) {
				pattern = supplier.get().map(Pattern::compile);
			}

			return assertNotNull(pattern).isPresent();
		}

		@Override
		public Pattern get() {
			if ( ! pattern.isPresent()) {
				pattern = supplier.get().map(Pattern::compile);
			}

			return assertNotNull(pattern).orElseThrow(() -> new IllegalStateException("Pattern is not present"));
		}
	}

	/**
	 *
	 * Retrieves the target artifact names from the control card content.
	 *
	 * @param outboundRegex the regex pattern supplier
	 * @param content the control card content
	 * @param outbound has parameter and value
	 * @return list of target artifact names in the control card
	 */
	private static List<String> getTargets(final LazyPatternSupplier outboundRegex, final String content, final Optional<UtilityOutbound> outbound) {
		/* Removes the control card continuation character in the content */
		final String normalizedContent = normalizeControlCards(content);
		if (outbound.isPresent() && ! outbound.get().getGroup().equals("0")) {
			final List<String> targets = JobControl.searchTargets(outboundRegex.get(), normalizedContent, Integer.parseInt(outbound.get().getGroup()));
			if (outbound.get().getTechnology().equals(Technology.SQL) && outbound.get().getType().equals(Type.TABLE)) {
				return targets.stream().map(JclJobContributor::removeSchemaFromSqlString).collect(Collectors.toList());
			}
			return targets;
		} else {
			return JobControl.searchTargetsInParentheses(outboundRegex.get(), normalizedContent);
		}
	}

	private static String normalizeControlCards(final String content) {
		final var matcher = CONTROL_CARD_TRAILING_NUMBERS.matcher(content);
		final String finalizedContent = matcher.find() ? matcher.replaceAll("") : content;
		return CNTL_CARD_CONTINUATION_CHAR.matcher(finalizedContent).replaceAll(" ");
	}

	/**
	 * Removes the schema from the sql table string. E.g., SCHEMA.TABLE becomes
	 * TABLE
	 *
	 * @param sqlTable the sql table string
	 * @return string without schema name
	 */
	private static String removeSchemaFromSqlString(final String sqlTable) {
		final var processedSqlTable = StringUtils.remove(sqlTable, "\"");
		final int dotIndex = StringUtils.indexOf(processedSqlTable, '.');
		return dotIndex == -1 || dotIndex + 1 == processedSqlTable.length() ? processedSqlTable : processedSqlTable.substring(dotIndex + 1);
	}

	private ModelAttributeMap<DatabaseAccessType> getDbAccessType(final String outboundRegex) {
		final var attributeMap = new ModelAttributeMap<DatabaseAccessType>();
		/* search the content with regex for possible readwrite attributes */
		if (PATTERN_READ_TABLE.matcher(outboundRegex).find()) {
			attributeMap.put(ModelAttributeKey.DB_ACCESS_TYPE, DatabaseAccessType.READ);
		} else if (PATTERN_STORE_TABLE.matcher(outboundRegex).find()) {
			attributeMap.put(ModelAttributeKey.DB_ACCESS_TYPE, DatabaseAccessType.STORE);
		} else if (PATTERN_UPDATE_TABLE.matcher(outboundRegex).find()) {
			attributeMap.put(ModelAttributeKey.DB_ACCESS_TYPE, DatabaseAccessType.UPDATE);
		} else if (PATTERN_DELETE_TABLE.matcher(outboundRegex).find()) {
			attributeMap.put(ModelAttributeKey.DB_ACCESS_TYPE, DatabaseAccessType.DELETE);
		} else {
			attributeMap.put(ModelAttributeKey.DB_ACCESS_TYPE, DatabaseAccessType.OTHER);
		}
		return attributeMap;
	}

	private void collectErrors(final ModuleBuilder rootModule, final JclParseResult jclParseResult, final List<JCL> listOfJcls, final UtilityList utilityList) {
		/* get parser errors + add assembled errors and create model error from each
		 * parser/assembled error
		 */
		listOfJcls.stream().flatMap(jcl -> jcl.getJclErrors().stream()).map(JclError::getErrorMessage)
				.map(errorMessage -> StringUtils.defaultIfBlank(errorMessage, DEFAULT_ERROR_MESSAGE))
				.forEach(errorMessage -> processErrorMessage(rootModule, utilityList, errorMessage, DUE_TO_MISSING_FILE, Severity.ERROR));
		jclParseResult.getAssembledJCLErrors().stream().map(JclError::getErrorMessage)
				.map(errorMessage -> StringUtils.defaultIfBlank(errorMessage, DEFAULT_ERROR_MESSAGE))
				.forEach(errorMessage -> processErrorMessage(rootModule, utilityList, errorMessage, MISSING_PROC_ERROR,
						isWarning(errorMessage) ? Severity.WARNING : Severity.ERROR));
	}

	private boolean isWarning(final String errorMessage) {
		return WARNING_MESSAGES.stream().anyMatch(errorMessage::contains);
	}

	/*
	 * This method will extract the procedure name from an error message, determine whether it's a utility,
	 * and only add the error message if it's not a utility.
	 */
	private void checkAndIgnoreUtilityErrors(final ModuleBuilder rootModule, final UtilityList utilityList, final String errorMessage) {
		final String[] errorParts = errorMessage.split(":");
		final String procName = errorParts[errorParts.length - 1].trim();
		if ( ! utilityList.isUtility(procName)) {
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMessage);
		}
	}
	
	private void processErrorMessage(final ModuleBuilder rootModule, final UtilityList utilityList, final String errorMessage, final String errorCondition,
			final Severity severity) {
		if (errorMessage.contains(errorCondition)) {
			checkAndIgnoreUtilityErrors(rootModule, utilityList, errorMessage);
		} else {
			rootModule.addError(severity, ErrorKey.PARSE_ERROR, errorMessage);
		}
	}
}
