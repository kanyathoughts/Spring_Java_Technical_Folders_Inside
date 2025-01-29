/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.cobol;

import com.google.common.collect.Streams;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol.DefaultCobolReferenceResolver;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.metrics.cobol.dependency.CobolDependencyNodeCollector;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.cobol.parser.ast.model.CobolExpression;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolOpenMode;
import innowake.ndt.cobol.parser.ast.model.CobolOpenOperand;
import innowake.ndt.cobol.parser.ast.statement.CobolOpenStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt.Access;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt.Organization;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFile;
import innowake.ndt.core.assembling.IAssembling;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Examines a target Cobol program, and detects all file READ or WRITE accessed
 * performed by the program. Also resolves the write accesses back to the actual
 * RESOURCE_FILE module and adds a dependency edge between the Cobol program and
 * the file.
 */
public class CobolReadWriteAccessAnalyzer {
	
	private static final Collection<Tuple2<Technology, Type>> RESOURCE_TYPES = List.of(Tuple2.of(Technology.RESOURCE, Type.FILE),
			Tuple2.of(Technology.RESOURCE, Type.VSAM_FILE),
			Tuple2.of(Technology.RESOURCE, Type.GDG_FILE));

	private static class FileAssignment {

		/** the name of a Cobol FD variable */
		@Nullable
		String fileDescriptorName;
		/**
		 * the symbolic name of the file which is either the name of the DD statement in
		 * JCL or the CICS CSD name of the file
		 */
		@Nullable
		String dataDefinitionName;
		/** the actual DSN that is assigned to the DD */
		@Nullable
		String dataSetName;
		/** condition (presence of modules in the call chain) under which the DSN is assigned to the DD */
		List<UUID> validIfReachedFrom = Collections.emptyList();
		/** whether the program reads from the file */
		boolean reads;
		/** whether the program writes to the file */
		boolean writes;

		@Nullable
		Access access;

		@Nullable
		Organization organization;
		
		boolean isASHyphenSet;
	}

	private static class ConditionalDsn {
		final String dsn;
		final List<UUID> validIfReachedFrom;

		public ConditionalDsn(final String dsn, final List<UUID> validIfReachedFrom) {
			this.dsn = dsn;
			this.validIfReachedFrom = validIfReachedFrom;
		}
	}
	
	private ParserProviderService parserProviderService;
	private final ModuleService moduleService;
	private final CallChainService callChainService;
	
	/* we need to collect all of these statements from the target module */
	private final List<CobolSelectStmt> selectStatements = new ArrayList<>();
	private final List<CobolOpenStmt> openStatements = new ArrayList<>();
	private final List<ExecCicsFile> cicsFileStatements = new ArrayList<>();

	public CobolReadWriteAccessAnalyzer(final ParserProviderService parserProviderService, final ModuleService moduleService,
			final CallChainService callChainService) {
		this.parserProviderService = parserProviderService;
		this.moduleService = moduleService;
		this.callChainService = callChainService;
	}
	
	/**
	 * Analyze the file access statements in Cobol file and creates dependency between CSD and Cobol through Resource file.
	 * 
	 * @param context the current discovery context
	 * @param sourceObject the {@link SourcePojo}
	 * @param builder a discovery builder for the source file
	 * @param moduleBuilder a module builder for the module
	 * @param module the {@link ModuleLightweightPojo}
	 */
	public void analyzeFileAccess(final DiscoveryContext context, final SourcePojo sourceObject, final DiscoveryBuilder builder,
			final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ModuleLightweightPojo module) {
		final var parseResult = parseCobolFile(context, sourceObject);
		final var collector = collectFileAccessStatements(parseResult.e1);
		final EntityId projectId = context.getProjectId();
		final var astNodeLocationProvider = new AstNodeLocationProvider<>(parseResult.e2, sourceObject.getContent().toString());
		final var referenceResolver = new DefaultCobolReferenceResolver(collector.getMoveStatements(), collector.getDataFields(), collector.getSetStmts(),
				sourceObject, astNodeLocationProvider);

		final List<FileAssignment> resolvedFileAssignmentsFromSelect = processSelectAndOpenStatements(module, context.getProjectId());
		final List<FileAssignment> fileAssignmentsFromCicsFileStatements = processCicsFileStatements(projectId, referenceResolver);
		/*
		 * combine file accesses FROM SELECT statements with file accesses from the CICS
		 * FILE statements, then for any files that were successfully resolved and that
		 * are either read or written, create a new dependency edge
		 */
		createDependencies(resolvedFileAssignmentsFromSelect, fileAssignmentsFromCicsFileStatements, moduleBuilder, builder);
	}
	
	private innowake.mining.shared.discovery.Tuple2<CobolModel, IAssembling<SourcePojo>> parseCobolFile(final DiscoveryContext context, final SourcePojo sourceObject) {
		final CobolParseResultProvider parseResultProvider = parserProviderService.createCobolParser(context);
		try {
			return parseResultProvider.getParseResult(sourceObject);
		} catch (final DiscoveryException e) {
			throw new IllegalStateException("Error while getting the cobol model: " + e);
		}
	}
	
	private CobolDependencyNodeCollector collectFileAccessStatements(final CobolModel cobolModel) {
	    final var collector = new CobolDependencyNodeCollector();
		collector.setDebugging(cobolModel.isDebugging());
	    collector.visit(cobolModel);
	    selectStatements.addAll(collector.getCobolSelectStatements());
	    openStatements.addAll(collector.getOpenStatements());
	    cicsFileStatements.addAll(collector.getExecCicsFiles());

	    return collector;
	}
	
	private List<FileAssignment> processSelectAndOpenStatements(final ModuleLightweightPojo module, final EntityId projectId) {
		List<FileAssignment> resolvedFileAssignmentsFromSelect = Collections.emptyList();
		if ( ! selectStatements.isEmpty() && ! openStatements.isEmpty()) {
			final MultiValuedMap<String, ConditionalDsn> ddToDsnMap = getDDtoDSNMapForModule(module, projectId);
			final Map<String, FileAssignment> fileAssignmentsFromSelect = collectFileAssignmentsFromSelectStatements();
			checkFileAccess(fileAssignmentsFromSelect);
			resolvedFileAssignmentsFromSelect = resolveDSN(fileAssignmentsFromSelect, ddToDsnMap);
		}
		return resolvedFileAssignmentsFromSelect;
	}
	
	private List<FileAssignment> processCicsFileStatements(final EntityId projectId, final DefaultCobolReferenceResolver referenceResolver) {
	    List<FileAssignment> fileAssignmentsFromCicsFileStatements = Collections.emptyList();
	    if (!cicsFileStatements.isEmpty()) {
	        fileAssignmentsFromCicsFileStatements = collectFileAssignmentsFromCicsFileStatements(projectId, referenceResolver);
	    }
	    return fileAssignmentsFromCicsFileStatements;
	}

	private Map<String, ModuleLightweightPojo> collectCSDMappings(final EntityId projId, final String name) {
		final Map<String, ModuleLightweightPojo> cicsFiles = new HashMap<>();
		
		moduleService.findModulesLightweight(q -> q.ofProject(projId).withTechnology(Technology.CSD).withType(Type.FILE).withName(name))
							.forEach(csdMod -> moduleService.findModulesLightweight(q2 -> q2.ofProject(projId)
																							.withSourceRelationshipsFrom(csdMod.identity())
																							.withTechnology(Technology.RESOURCE)
																							.withType(Type.FILE))
																.forEach(csdModOutFile -> cicsFiles.put(csdMod.getName(), csdModOutFile)));

		return cicsFiles;
	}
	
	private List<FileAssignment> collectFileAssignmentsFromCicsFileStatements(final EntityId projectId, final DefaultCobolReferenceResolver referenceResolver) {
		final Map<String, FileAssignment> fileAssignments = new HashMap<>();
		cicsFileStatements.forEach(cicsFileStatement -> {

			final String resolvedName = referenceResolver.resolve(cicsFileStatement);
			final var resolvedModule = getCicsOrResourceModule(projectId, resolvedName);
			if (resolvedModule == null) {
				return;
			}
			final String fileName = resolvedModule.getName();
			final FileAssignment fileAssignment;
			if (fileAssignments.containsKey(fileName)) {
				fileAssignment = fileAssignments.get(fileName);
			} else {
				final Map<String, ModuleLightweightPojo> cicsFiles = collectCSDMappings(resolvedModule.getProject(), fileName);
				final ModuleLightweightPojo targetFile = cicsFiles.get(fileName);
				if (targetFile == null) {
					return;
				}
				fileAssignment = new FileAssignment();
				fileAssignment.dataDefinitionName = fileName;
				fileAssignment.dataSetName = targetFile.getName();
				fileAssignments.put(fileName, fileAssignment);
			}

			switch (cicsFileStatement.getOperation()) {
				case READ:
				case READNEXT:
				case READPREV:
				case STARTBR:
				case ENDBR:
				case RESETBR:
					fileAssignment.reads = true;
					break;
				case WRITE:
				case REWRITE:
				case DELETE:
					fileAssignment.writes = true;
					break;
				case UNLOCK:
					break;
				default:
					break;
			}
		});
		return new ArrayList<>(fileAssignments.values());
	}

	@Nullable
	private ModulePojo getCicsOrResourceModule(final EntityId projectId, final String resolvedName) {
		final Optional<ModulePojo> module = moduleService.findModules(b -> b.ofProject(projectId).withName(resolvedName)).stream().filter(e -> 
		(e.getTechnology().equals(ModuleType.CSD_FILE.getTechnology()) && e.getType().equals(ModuleType.CSD_FILE.getType()))
				|| (e.getTechnology().equals(ModuleType.RESOURCE_FILE.getTechnology()) && e.getType().equals(ModuleType.RESOURCE_FILE.getType()))).findFirst();

		return module.orElse(null);
	}
	
	private void createDependencies(final List<FileAssignment> fileAssignmentsFromSelect, final List<FileAssignment> fileAssignmentsFromExecCics,
			final ModuleBuilder moduleBuilder, final DiscoveryBuilder builder) {
		Streams.concat(fileAssignmentsFromSelect.stream(), fileAssignmentsFromExecCics.stream()).forEach(fileAssignment -> {
			if (!(fileAssignment.reads || fileAssignment.writes)) {
				/*
				 * the program contains an FD for the file, but neither reads nor writes the
				 * file, so we're not adding a dependency
				 */
				return;
			}

			if (fileAssignment.dataSetName == null || StringUtils.isBlank(fileAssignment.dataSetName)) {
				/*
				 * was unable to resolve the name of the dataset, so can not add dependency ->
				 * create ModelError instead
				 */
				moduleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
						"Unable to resolve file " + fileAssignment.dataDefinitionName + " to actual data set.");
				return;
			}

			final List<Object> accessTypes = new ArrayList<>();

			if (fileAssignment.reads) {
				accessTypes.add(ModelAttributeValue.FileAccess.READ);
			}
			if (fileAssignment.writes) {
				accessTypes.add(ModelAttributeValue.FileAccess.WRITE);
			}

			final var dependencyBuilder = moduleBuilder.declareDependency(RelationshipType.ACCESSES,
					new ModuleFilter().setNames(assertNotNull(fileAssignment.dataSetName))
							.setTypes(ModuleType.RESOURCE_FILE, ModuleType.RESOURCE_VSAM_FILE, ModuleType.RESOURCE_GDG_FILE),
					ResolutionFlag.MERGE_DUPLICATES);

			if ( ! fileAssignment.validIfReachedFrom.isEmpty()) {
				final List<ModuleFilter> moduleFilters = fileAssignment.validIfReachedFrom.stream()
						.map(uuid -> new ModuleFilter().setModuleIds(EntityId.of(uuid)))
						.toList();
				dependencyBuilder.setReachedFromModules(moduleFilters);
			}

			dependencyBuilder.setBinding(Binding.LATE);

			if (fileAssignment.organization == Organization.INDEXED || fileAssignment.organization == Organization.RELATIVE
					|| (fileAssignment.organization == Organization.SEQUENTIAL && fileAssignment.isASHyphenSet)) {
				builder.anchorTo(new ModuleFilter().setNames(assertNotNull(fileAssignment.dataSetName)).setTypes(ModuleType.RESOURCE_FILE))
						.andResetType(ModuleType.RESOURCE_VSAM_FILE);
			}

			addAdditionalAttributes(fileAssignment, accessTypes, dependencyBuilder);
		});
	}

	private void addAdditionalAttributes(final FileAssignment fileAssignment, final List<Object> accessTypes,
			final DependencyBuilder dependencyBuilder) {
		dependencyBuilder.addAttribute(ModelAttributeKey.FILE_ACCESS_TYPE, accessTypes);

		if (fileAssignment.fileDescriptorName != null) {
			dependencyBuilder.addAttribute(ModelAttributeKey.COBOL_FD_NAME, fileAssignment.fileDescriptorName);
		}

		if (fileAssignment.dataDefinitionName != null) {
			dependencyBuilder.addAttribute(ModelAttributeKey.FILE_ALIAS, fileAssignment.dataDefinitionName);
		}

		if (fileAssignment.organization != null) {
			dependencyBuilder.addAttribute(ModelAttributeKey.ORGANIZATION, fileAssignment.organization);
		}

		if (fileAssignment.access != null) {
			dependencyBuilder.addAttribute(ModelAttributeKey.ACCESS_MODE, fileAssignment.access);
		}
	}
	
	private MultiValuedMap<String, ConditionalDsn> getDDtoDSNMapForModule(final ModuleLightweightPojo module, final EntityId projectId) {
		final Set<EntityId> callingSteps = new HashSet<>();

		final Optional<List<CallChainGraph>> callChains = callChainService.createCallChainGraphs(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.setStartModuleIds(Collections.singletonList(module.identity()))
				.setCallTypes(Collections.singleton(RelationshipType.CALLS))
				.setDirections(Collections.singletonList(CallChain.CallChainDirection.IN))
				.setEndModuleTypes(Collections.singleton(Type.EXEC_PGM))
				.setParallel(1)
				.build());

		callChains.ifPresent(callChainGraphs ->
				callChainGraphs.stream()
				.flatMap(graph -> graph.getEndModules(endModule -> Type.EXEC_PGM == endModule.getType()).stream())
				.forEach(endModule -> callingSteps.add(endModule.identity())));

		final MultiValuedMap<String, ConditionalDsn> ddToDsnMap = new HashSetValuedHashMap<>();

		callingSteps.stream()
			.flatMap(stepModuleId -> moduleService.findRelationship(q -> q.ofSource(stepModuleId)).stream())
			.forEach(dep -> {
				final Optional<ModuleLightweightPojo> called = moduleService.findAnyModuleLightweight(q -> q.byUid(dep.getDstModule())
															 												.withTechnologiesAndTypes(RESOURCE_TYPES));
				if (called.isPresent()) {
					final var properties = dep.getProperties();
					if (properties.isPresent()) {
						@SuppressWarnings("unchecked")
						final List<Map<String, String>> propertiesList = (List<Map<String, String>>) properties.get().get("PROPERTIES");
						final String ddName = propertiesList.get(0).get(JobControl.ID_NAME);
						final String dsn = called.get().getName();
						if (dep.getValidIfReachedFrom().isEmpty()) {
							/* If no condition exists on the file dependency on the step, then add the step module itself as condition
							 * for the file dependency on the Module: file dependency is valid only if the Module is called from this step */
							ddToDsnMap.put(ddName, new ConditionalDsn(dsn, List.of(dep.getSrcModule())));
						} else {
							/* If the file dependency on the step already has a condition, then copy that condition to the file dependency on the module.
							 * Actually, we would have to add the step module as a second condition, but "valid if reached from" does not support 'AND'
							 * logic at the moment. */
							ddToDsnMap.put(ddName, new ConditionalDsn(dsn, dep.getValidIfReachedFrom()));
						}

					}
				}
			});

		return ddToDsnMap;
	}

	private Map<String, FileAssignment> collectFileAssignmentsFromSelectStatements() {
		final Map<String, FileAssignment> fileAssignments = new HashMap<>();

		selectStatements.forEach(selectStmt -> {
			final var fileAssignment = new FileAssignment();
			fileAssignment.fileDescriptorName = selectStmt.getValue().getText();
			final String ddName = MetricsUtility.trimQuotesSpaces(selectStmt.getAssignment().toString());
			fileAssignment.dataDefinitionName = ddName.substring(ddName.lastIndexOf("-") + 1);
			if (ddName.contains("AS-")) {
				fileAssignment.isASHyphenSet = true;
			}
			fileAssignment.organization = selectStmt.getOrganization() != null ? selectStmt.getOrganization() : Organization.SEQUENTIAL;
			fileAssignment.access = selectStmt.getAccess() != null ? selectStmt.getAccess() : Access.SEQUENTIAL;
			fileAssignments.put(fileAssignment.fileDescriptorName, fileAssignment);
		});

		return fileAssignments;
	}

	private void checkFileAccess(final Map<String, FileAssignment> fileAssignments) {
		for (final CobolOpenStmt openStmt : openStatements) {
			for (final CobolOpenOperand operand : openStmt.getOperands()) {
				addFileAccess(operand, fileAssignments);
			}
		}
	}

	private void addFileAccess(final CobolOpenOperand operand, final Map<String, FileAssignment> fileAssignments) {

		final CobolOpenMode openMode = operand.getIntent();
		if (openMode != null) {

			for (final CobolExpression fileExpression : operand.getOperands()) {
				final var fileAssignment = fileAssignments.get(MetricsUtility.trimQuotesSpaces(fileExpression.toString()));
				if (fileAssignment == null) {
					continue;
				}

				switch (openMode) {
					case INPUT:
						fileAssignment.reads = true;
						break;
					case OUTPUT:
					case EXTEND:
						fileAssignment.writes = true;
						break;
					case IO:
						fileAssignment.reads = true;
						fileAssignment.writes = true;
				}
			}
		}

	}

	private List<FileAssignment> resolveDSN(final Map<String, FileAssignment> fileAssignmentsFromSelect, final MultiValuedMap<String, ConditionalDsn> ddToDsnMap) {

		return fileAssignmentsFromSelect.values().stream().flatMap(fileAssignment -> {
			if ( ! ddToDsnMap.containsKey(fileAssignment.dataDefinitionName)) {
				/* DSN not resolved */
				return Stream.of(fileAssignment);
			}
			return ddToDsnMap.get(fileAssignment.dataDefinitionName).stream().map(conditionalDsn -> {
				/*
				 * there may be multiple DSN assignments for this file declaration - duplicate
				 * the file declaration object for each
				 */
				final var copiedAssignment = new FileAssignment();
				copiedAssignment.dataDefinitionName = fileAssignment.dataDefinitionName;
				copiedAssignment.isASHyphenSet = fileAssignment.isASHyphenSet;
				copiedAssignment.fileDescriptorName = fileAssignment.fileDescriptorName;
				copiedAssignment.reads = fileAssignment.reads;
				copiedAssignment.writes = fileAssignment.writes;
				copiedAssignment.organization = fileAssignment.organization;
				copiedAssignment.access = fileAssignment.access;
				copiedAssignment.dataSetName = conditionalDsn.dsn;
				copiedAssignment.validIfReachedFrom = conditionalDsn.validIfReachedFrom;
				return copiedAssignment;
			});
		}).toList();
	}
}
