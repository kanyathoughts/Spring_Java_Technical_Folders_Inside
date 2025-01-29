/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.sql.SqlTableExtractor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.ndt.jcl.parser.api.IDDModelInternal;
import innowake.ndt.jcl.parser.model.Step;
import innowake.ndt.jcl.parser.model.StepExec;
import innowake.ndt.jcl.parser.model.StepIf;

/**
 * Common methods for JCL
 */
public class JobControl {

	private static final Logger LOG = LoggerFactory.getLogger(JobControl.class);
	private static final Pattern INCLUDE_PATTERN = Pattern.compile("INCLUDE +MEMBER *= *([\\S]+)");
	public static final Pattern GDG_FILE_PATTERN = Pattern.compile("^[+\\-]\\d+$|^0$");
	private static final Pattern BEGIN_PROC_PATTERN = Pattern.compile("<\\$ +BEGIN +OF +PROC +(.*?) +\\$>");
	public static final String PROGRAM = "PGM";
	public static final String PROC = "PROC";
	public static final String PARAMETER = "PARM";
	public static final String EZT_LOADER = "EZTPA00";  /* EZTPA00 is the EZT loader program */
	public static final String SYSIN = "SYSIN";
	public static final String DSN = "DSN";
	public static final String DSNAME = "DSNAME";
	public static final String ID_NAME = "ID_NAME"; /* as in Identifier Name of statement ("//[ID_NAME] ...")*/
	public static final String JOBLIB = "JOBLIB";
	public static final String STEPLIB = "STEPLIB";
	private final SourceService sourceService;
	private DiscoveryBuilderFromSource builder;
	private DiscoveryContext context;
	private ParserProviderService parserProvider;

	public JobControl(final SourceService sourceService, final ParserProviderService parserProvider, final DiscoveryBuilderFromSource builder,
			final DiscoveryContext context) {
		this.sourceService = sourceService;
		this.builder = builder;
		this.context = context;
		this.parserProvider = parserProvider;
	}

	/**
	 * Return all references in INCLUDE MEMBER statements
	 *
	 * @param jobContents string representation of the contents of the jcl file.
	 * @return a set of include group/references in the jcl
	 */
	public static Set<String> findIncludeMembers(final String jobContents) {
		/* Parser does not return include statements */
		final Set<String> includeGroups = new HashSet<>();
		for (final String line : jobContents.split("\n")) {
			/* ignore comments */
			if ( ! line.startsWith("//*")) {
				final Matcher m = INCLUDE_PATTERN.matcher(line);
				while (m.find()) {
					includeGroups.add(m.group(1));
				}
			}
		}

		return includeGroups;
	}

	/**
	 * Filters DDs if current DD in the List of dependencies that needs to be ignored.
	 * For example SYSDUMP, SYSABEND.....etc.
	 * 
	 * @param config Config information
	 * @return a predicate that tests if current DD need to ignored
	 */	
	public static Predicate<IDDModelInternal> filterDDsInIgnoreDependencyList(final Config config) {
		return dd -> ! config.getDDIgnoreDependencyList().contains(dd.getId());
	}
	
	/**
	 * Check if resource is a PS file control card.
	 * 
	 * @param config JCL Config.
	 * @param dd DD associated with the step
	 * @param dsnName the name associated with DSN or DSNAME in DD
	 *
	 * @return true if resource is a PS file control card.
	 */
	public static boolean isPsFileControlCard(final Config config, final IDDModelInternal dd, final String dsnName) {
		return config.getPgmStepNameList().contains(dd.getId()) && dsnName.indexOf('(') < 0 && dsnName.indexOf(')') < 0;
	}

	/**
	 * Fetches the control card content for the given DD name
	 *
	 * @param stepExec the step for which the dd should be searched for
	 * @param ddName the name of the dd to fetch the control card content
	 * @return the control card content
	 */
	public String getControlCardContent(final StepExec stepExec, final String ddName) {
		if ( ! stepExec.getDDs().containsKey(ddName)) {
			return "";
		}
		return stepExec.getDDs().get(ddName).stream().map(dd -> {
			final String controlCard = getControlCardName(dd);
			if (StringUtils.isNotBlank(controlCard)) {
				try {
					return sourceService.get(x -> x.ofProject(context.getProjectId()).withName(controlCard).withType(Type.CONTROLCARD)).getContent().toString();
				} catch (final Exception e) {
					LOG.error("An error occurred while fetching the content for the control card named: " + controlCard, e);
					return StringUtils.EMPTY;
				}
			} else if ( ! StringUtils.isBlank(dd.getProperties().get("*"))) {
				return dd.getProperties().get("*");
			}
			return "";
		}).collect(Collectors.joining());
	}

	/**
	 * Fetches the control card content a list for the given DD name
	 *
	 * @param stepExec the step for which the dd should be searched for
	 * @param ddName the name of the dd to fetch the control card content
	 * @return the control card content list
	 */
	public List<String> getControlCardContentList(final StepExec stepExec, final String ddName, final ModuleBuilder module) {
		if ( ! stepExec.getDDs().containsKey(ddName)) {
			return Collections.emptyList();
		}
		return stepExec.getDDs().get(ddName).stream().map(dd -> {
			final String controlCard = getControlCardName(dd);
			if (StringUtils.isNotBlank(controlCard)) {
				try {
					return sourceService.get(x -> x.ofProject(context.getProjectId()).withName(controlCard).withType(Type.CONTROLCARD)).getContent().toString();
				} catch (final Exception e) {
					final var message = " An error occurred while fetching the content for the control card named: ";
					LOG.error(message + controlCard, e);
					module.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
							message + controlCard);
					return StringUtils.EMPTY;
				}
			} else if ( ! StringUtils.isBlank(dd.getProperties().get("*"))) {
				return dd.getProperties().get("*");
			}
			return StringUtils.EMPTY;
		}).filter(StringUtils::isNotBlank).collect(Collectors.toList());
	}

	/**
	 * Extract tables with it access type from the SQLs in the control cards.
	 *
	 * @param config        the Config
	 * @param projectId     the project id
	 * @param step          the JCL step
	 * @param stepSubModule the module builder for the step
	 * @return the map of table name and its access type
	 */
	public Map<String, List<DatabaseAccessType>> collectTablesInStep(final Config config, final EntityId projectId, final Step step,
			final ModuleBuilder stepSubModule) {
		final List<IDDModelInternal> internal = step.getDDs().get(SYSIN);
		final String program = step.getProperties().get(PROGRAM);
		if (program == null || internal == null || program.equals(EZT_LOADER)) {
				return Collections.emptyMap();
		}
		final var sqlList = getControlCardContentList((StepExec) step, SYSIN, stepSubModule);
		/* We have to also collect the combined string of all control cards as there are scenarios where queries span across multiple control cards*/
		if (sqlList.size() > 1) {
			sqlList.add(String.join(" ", sqlList));
		}
		final Set<String> statements = new HashSet<>();
		/* Extract tables and collect the SQL statements in one go */
		final var tables = sqlList.stream().map(sql -> SqlTableExtractor.extract(stepSubModule, sql)).flatMap(result -> {
				statements.addAll(result.getSecond());
				return result.getFirst().entrySet().stream();})
				.collect(Collectors.toMap(
						Map.Entry::getKey,
						Map.Entry::getValue, ListUtils::sum));
		statements.forEach(statementsString -> DawnExecStatementUtility.addSqlStatementForExecSql(statementsString, stepSubModule, true));
		return tables;
	}

	/**
	 * Returns all IF steps that are in the provided list and
	 * recursively any child IF steps.
	 * @param steps The list of steps.
	 *
	 * @return The list of IF steps.
	 */
	public static List<StepIf> getAllStepIf(final List<Step> steps) {
		final List<StepIf> result = new ArrayList<>();

		for (final Step step : steps) {
			result.addAll(getAllStepIf(step.getSteps()));
			if (step instanceof StepIf) {
				final StepIf stepIf = (StepIf) step;
				result.add(stepIf);
				result.addAll(getAllStepIf(stepIf.getIfNode().getSteps()));
				result.addAll(getAllStepIf(stepIf.getElseNode().getSteps()));
			}
		}
		return result;
	}

	/**
	 * Returns all immediate children of a StepIf.
	 * @param ifStep The if step.
	 * @return The list of Step.
	 */
	public static List<Step> getIfStepChildSteps(final StepIf ifStep) {
		final List<Step> result = new ArrayList<>();
		result.addAll(ifStep.getIfNode().getSteps());
		result.addAll(ifStep.getElseNode().getSteps());
		return result;
	}

	/**
	 * This method creates the list of list of DD by their JCL statement name
     * by looping on all the steps.
	 *
	 * @param steps List of all steps
	 * @return list of list of all DDs associated with the step
	 */
	public static List<List<IDDModelInternal>> getAllDDs(final List<Step> steps) {
		final List<List<IDDModelInternal>> dds = new ArrayList<>();
		for (final Step step : steps) {
			dds.addAll(step.getDDs().values());
		}
		return dds;
	}

	/**
	 * Returns the control card name in DD associated with DSN or DSNAME.
	 * <pre>
	 * {@code // DD DSN=TEST.PDS(CARD1)
	 * // DD DSN=TEST.PDS(CARD2)
	 * // DD DSN=TEST.CARD3
	 * // DD DSN=CARD4}</pre>
	 * Here CARD1, CARD2, TEST.CARD3 AND CARD4 are extracted as control cards.
	 *
	 * @param dd DD associated with the step
	 * @return the control card name associated with DSN or DSNAME in DD
	 */
	public static String getControlCardName(final IDDModelInternal dd) {
		final Map<String, String> ddProperties = dd.getProperties();

		/* the dd passed should have DSN and DSNAME */
		if (ddProperties.get(DSN) != null || ddProperties.get(DSNAME) != null) {
			final String line = dd.getProperties().getOrDefault(DSN, dd.getProperties().get(DSNAME));
			final Optional<String> memberName = getNameInParentheses(line);
			/* This will fix issue where the version number of the GDG file is extracted as a control card name */
			if (memberName.isPresent() && GDG_FILE_PATTERN.matcher(memberName.get()).find()) {
				return StringUtils.EMPTY;
			}
			return memberName.isEmpty() ? line : memberName.get();
		}
		return StringUtils.EMPTY;
	}

	/**
	 * Returns the step recursively in the jcl.
	 *
	 * @param step The step in the jcl
	 * @return The stream of step
	 */
	public static Stream<Step> getJCLStepsRecursively(final Step step) {
		final List<Step> steps = step.getSteps();
		return Stream.concat(steps.stream(), steps.stream().flatMap(JobControl::getJCLStepsRecursively));
	}
	
	/**
	 * Used to identify if the repository entry is parsable.
	 * Only type of {@code Type#JOB} and {@code Type#PROC} are parsable.
	 * For example {@code Type#CONFIG} is not.
	 *
	 * @param sourceObject The entry to check the parsability.
	 * @return true if parsable, otherwise false.
	 */
	public static boolean isParsable(final SourcePojo sourceObject) {
		return sourceObject.getType() == Type.JOB || sourceObject.getType() == Type.PROC;
	}

	/**
	 * Used to get the {@code ResolveTarget} enum for the step exec instance.
	 *
	 * @param step The step exec instance.
	 * @return The resolve target enum.
	 * <br>One of
	 * <li>{@code ResolveTarget#JCL_EXEC}
	 * <li>{@code ResolveTarget#JCL_EXEC_PGM}
	 */
	static ModuleType getExecType(final StepExec step) {
		return step.isExecProc() ? ModuleType.JCL_EXEC : ModuleType.JCL_EXEC_PGM;
	}
	
	/**
	 * Used to get the target name of a job STEP EXEC.
	 * Depending on if the target is a PROC or PGM the target is read from the properties.
	 * The Optional result may be not present if the property is not set.
	 *
	 * @param exec The step to read the target from.
	 * @return The optional string present and set if the property is found, otherwise not present.
	 */
	static Optional<String> getStepExecTargetName(final StepExec exec) {
		if (exec.isExecProc()) {
			return Optional.of(exec.getProperties().get(PROC));
		} else {
			return Optional.of(exec.getProperties().get(PROGRAM));
		}
	}

	static Optional<String> getNameInParentheses(final String line) {
		final int openIndex = line.indexOf('(');
		final int closeIndex = line.indexOf(')');
		if (openIndex > 0 && closeIndex > 0) {
			return Optional.ofNullable(line.substring(openIndex + 1, closeIndex));
		}
		return Optional.empty();
	}

	static int countMatch(final Pattern pattern, final String rawString) {
		final Matcher matcher = pattern.matcher(rawString);
		int count = 0;
		while (matcher.find()) {
			count++;
		}
		return count;
	}

	static List<String> searchTargetsInParentheses(final Pattern pattern, final String rawString){
		final Matcher matcher = pattern.matcher(rawString);
		final List<String> targets = new ArrayList<>();
		while (matcher.find()) {
			final Optional<String> name = getNameInParentheses(matcher.group());
			name.ifPresent(targets::add);
		}
		return targets;
	}

	/**
	 *  Matches the given {@code rawString} against a {@code pattern} and returns a {@link List} of target names captured by the entire {@code pattern}.
	 *
	 * @param pattern the {@link Pattern} to match the {@code rawString} against
	 * @param rawString the {@link String} to match
	 * @return the matched target names
	 */
	static List<String> searchTargets(final Pattern pattern, final String rawString){
		return searchTargets(pattern, rawString, 0);
	}

	/**
	 *  Matches the given {@code rawString} against a {@code pattern} and returns a {@link List} of target names captured by the given {@code group}.
	 *
	 * @param pattern the {@link Pattern} to match the {@code rawString} against
	 * @param rawString the {@link String} to match
	 * @param group the group to capture; see {@link Matcher#group(int)}
	 * @return the matched target names
	 */
	static List<String> searchTargets(final Pattern pattern, final String rawString, final int group){
		final Matcher matcher = pattern.matcher(rawString);
		final List<String> targets = new ArrayList<>();
		while (matcher.find()) {
			targets.add(matcher.group(group));
		}
		return targets;
	}

	static String assembleKeywordAndValue(final String key, final String value) {
		return key +"=("+ value+")";
	}
	
	/**
	 * Collects the inline procs modules
	 *
	 * @param sourceObject the sourceObject
	 * @param jclContents the jclContents
	 * @param assembledJcl the assembledJcl
	 * @param inlineProcs captures the list of inline procs in the current file
	 */
	public void collectInlineProcs( final SourcePojo sourceObject, final String jclContents, final String assembledJcl, final Set<String> inlineProcs) {
			/* Regex to return all characters between "BEGIN OF PROC and END OF PROC or beginning of a new BEGIN OF PROC"
			 * Pay careful attention to the use of greedy character matching (.*?) which ensures we get the first match
			 * that we find (and don't overshoot).
			 */
			final Matcher mBeg = BEGIN_PROC_PATTERN.matcher(assembledJcl);
			while (mBeg.find()) {
				final String procName = mBeg.group(1);

				/* If we find proc definition in unassembled jcl then it must be inline proc */
				final Pattern pattern = Pattern.compile("//" + procName + " +PROC(.*?)// +PEND *$", Pattern.DOTALL | Pattern.MULTILINE);
				if (pattern.matcher(jclContents).find()) {
					/* validated and can add to final list, append batch base name to procname so that it's unique from external/catalogued procs */
					final String inlineProcName = FilenameUtils.getBaseName(sourceObject.getPath()) + "." + procName;
					if ( ! inlineProcs.contains(inlineProcName)) {
					builder.declareSubModule(inlineProcName, ModuleType.JCL_INLINE_PROC);
					inlineProcs.add(inlineProcName);
					}
				}
			}
	}

	/**
	 * collect the IncludeGroups
	 *
	 * @param jclContents the jclContents
	 * @param rootModule the root module
	 */
	public void collectIncludeGroups(final String jclContents, final ModuleBuilder rootModule) {
		final Set<String> includeMembers = JobControl.findIncludeMembers(jclContents);
		if ( ! includeMembers.isEmpty()) {
			includeMembers.stream().forEach(includeMemberName -> {
				rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames(includeMemberName).setTypes(ModuleType.JCL_INCLUDE))
							.setBinding(Binding.LATE);
			});
		}
	}
	
	/**
	 * Collects Easytrieve modules and dependencies
	 *
	 * @param steps the jcl steps
	 * @param rootModule the root module
	 */
	public void collectEasytrieve(final List<Step> steps, final ModuleBuilder rootModule) {
		final Supplier<Stream<IDDModelInternal>> dds = () -> steps.stream().filter(step -> step.getProperties().get(JobControl.PROGRAM) != null)
				.filter(step -> step.getProperties().get(JobControl.PROGRAM).equals(JobControl.EZT_LOADER))
				.filter(step -> step.getDDs().get(JobControl.SYSIN) != null).flatMap(step -> step.getDDs().get(JobControl.SYSIN).stream());

		/* collect inline easytrieve program dependencies */
		final List<String> inlineEasytrive = dds.get().filter(dd -> dd.getProperties().get("*") != null)
				.filter(dd -> !dd.getProperties().get("*").trim().isEmpty()).map(dd -> dd.getProperties().get("*")).collect(Collectors.toList());
		new BatchEasytrieveContributor(parserProvider, builder, context, rootModule).collectMetrics(inlineEasytrive.toArray(new String[] {}));
	}

}
