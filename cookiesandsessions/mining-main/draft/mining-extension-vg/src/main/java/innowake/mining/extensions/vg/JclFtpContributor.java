/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.vg;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclContributorContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclUtilityContributor;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.jcl.parser.model.StepExec;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Contributor for FTP utility
 */
@Component
public class JclFtpContributor implements JclUtilityContributor {

	private static final String REMOTE_FILE = "remoteFile";
	private static final String DD_KEYWORD = "//DD:";
	private static final String LOCAL_FILE = "localFile";
	private static final String REMOTE_DIRECTORY = "remoteDirectory";
	private static final String OUTPUT_DD = "OUTPUT";
	private static final Logger LOG = LoggerFactory.getLogger(JclFtpContributor.class);
	private static final List<String> ACTION_LINES = List.of("GET ", "PUT ", "APPEND ", "ASCII", "BINARY", "CD ", "DELETE ", "DIR", "LS ", "MDIR ", "MGET ",
			"MPUT ", "MDELETE");

	/**
	 * Accepts only FTP utility steps
	 *
	 * @param stepExec the step to check
	 * @return {@code true} if the contributor can process this step
	 */
	@Override
	public boolean accept(final StepExec stepExec) {
		return "FTP".equals(stepExec.getProperties().get("PGM"));
	}

	@Override
	public void contribute(final JclContributorContext context, final StepExec stepExec, final DiscoveryBuilder.ModuleBuilder stepModule) {
		try {
			final String sysinContent = context.getJobControl().getControlCardContent(stepExec, "SYSIN");
			final String inputContent = context.getJobControl().getControlCardContent(stepExec, "INPUT");
			final String controlCardContent = StringUtils.isNotBlank(sysinContent) ? sysinContent : inputContent;

			if (StringUtils.isBlank(controlCardContent)) {
				stepModule.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR,
						"No control card content found for FTP utility in " + stepExec.toString());
				return;
			}
			/* Split the control card content into lines and filter out empty lines */
			final List<String> lines = Arrays.stream(controlCardContent.split("\\r?\\n")).filter(StringUtils::isNotBlank).collect(Collectors.toList());

			final Map<Integer, String> cdLines = getStatements(lines, "CD ");

			final Map<String, Object> properties = new HashMap<>();
			/* Assume that the first 3 lines contains server, username and password */
			if (lines.size() > 3 && isNotActionLine(lines.get(0)) && isNotActionLine(lines.get(1)) && isNotActionLine(lines.get(2))) {
				properties.put("server", lines.get(0));
				properties.put("user", lines.get(1));
				properties.put("password", lines.get(2));
			} else {
				stepModule.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR,
						"Could not determine server, username & password from : " + controlCardContent);
			}

			properties.put("get", getPropertiesForGetStatement(cdLines, lines, stepModule, stepExec, context));
			properties.put("put", getPropertiesForPutStatement(cdLines, lines, stepModule, stepExec, context));
			properties.put("append", getPropertiesForAppendStatement(cdLines, lines, stepModule, stepExec, context));
			properties.put("listCatalog", getPropertiesForListCatalogStatement(cdLines, lines, stepExec));

			stepModule.declareStatement(StatementType.FTP).setProperties(properties).setText(controlCardContent).setTechnology(Technology.JCL);

		} catch (final Exception e) {
			LOG.warn("Unable to resolve metrics for FTP utility in " + stepExec.toString(), e);
			stepModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR,
					"Unable to resolve metrics for FTP utility in " + stepExec.toString() + ": " + e.getMessage());
		}
	}

	private static boolean isNotActionLine(final String line) {
		return ACTION_LINES.stream().noneMatch(line::startsWith);
	}

	private static List<Map<String, String>> getPropertiesForGetStatement(final Map<Integer, String> cdLines, final List<String> controlCardContent,
			final DiscoveryBuilder.ModuleBuilder stepModule, final StepExec stepExec, final JclContributorContext context) {
		final Map<Integer, String> getLines = getStatements(controlCardContent, "GET ");
		final List<Map<String, String>> getStmts = new ArrayList<>();
		getLines.forEach((lineNumber, statement) -> {
			final String[] splitGetStatement = removeQuotesAndEmptyStrings(statement.split(" "));
			if (splitGetStatement.length < 2) {
				stepModule.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR, "Invalid GET statement: " + statement);
				return;
			}
			final Map<String, String> getStmt = new HashMap<>();
			getStmt.put(REMOTE_FILE, MetricsUtility.trimQuotesSpaces(splitGetStatement[0]));
			String localFile = MetricsUtility.trimQuotesSpaces(splitGetStatement[1]);
			if (localFile.startsWith(DD_KEYWORD) && localFile.length() > 5) {
				final String ddLocalFile = localFile.substring(5);
				if (stepExec.getDDs().containsKey(ddLocalFile)) {
					localFile = stepExec.getDDs().get(ddLocalFile).stream().map(JobControl::getControlCardName).collect(Collectors.joining(", "));
				}
			} else {
				context.getDiscoveryBuilder().declareExternalModule(localFile, ModuleType.RESOURCE_FILE);
				final ModuleFilter externalModuleFilter = new ModuleFilter().setNames(localFile).setTypes(ModuleType.RESOURCE_FILE);
				final var dependency = stepModule.declareDependency(RelationshipType.ACCESSES, externalModuleFilter, ResolutionFlag.MERGE_DUPLICATES);
				context.getConditionalDependency().ifPresent(x -> x.accept(dependency));
			}
			getStmt.put(LOCAL_FILE, localFile);

			final Integer immediateCDStatement = getImmediateCDStatement(cdLines, lineNumber);
			if (immediateCDStatement != null) {
				getStmt.put(REMOTE_DIRECTORY, cdLines.get(immediateCDStatement));
			}
			getStmts.add(getStmt);
		});

		return getStmts;
	}

	private static List<Map<String, String>> getPropertiesForPutStatement(final Map<Integer, String> cdLines, final List<String> controlCardContent,
			final DiscoveryBuilder.ModuleBuilder stepModule, final StepExec stepExec, final JclContributorContext context) {
		final Map<Integer, String> putLines = getStatements(controlCardContent, "PUT ");
		final List<Map<String, String>> putStmts = new ArrayList<>();
		putLines.forEach((lineNumber, statement) -> {
			final String[] splitPutStatement = removeQuotesAndEmptyStrings(statement.split(" "));
			if (splitPutStatement.length < 2) {
				stepModule.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR, "Invalid PUT statement: " + statement);
				return;
			}
			final Map<String, String> putStmt = new HashMap<>();
			putStmt.put(REMOTE_FILE, MetricsUtility.trimQuotesSpaces(splitPutStatement[1]));
			String localFile = MetricsUtility.trimQuotesSpaces(splitPutStatement[0]);
			if (localFile.startsWith(DD_KEYWORD) && localFile.length() > 5) {
				final String ddLocalFile = localFile.substring(5);
				if (stepExec.getDDs().containsKey(ddLocalFile)) {
					localFile = stepExec.getDDs().get(ddLocalFile).stream().map(JobControl::getControlCardName).collect(Collectors.joining(", "));
				}
			} else {
				context.getDiscoveryBuilder().declareExternalModule(localFile, ModuleType.RESOURCE_FILE);
				final ModuleFilter externalModuleFilter = new ModuleFilter().setNames(localFile).setTypes(ModuleType.RESOURCE_FILE);
				final var dependency = stepModule.declareDependency(RelationshipType.ACCESSES, externalModuleFilter, ResolutionFlag.MERGE_DUPLICATES);
				context.getConditionalDependency().ifPresent(x -> x.accept(dependency));
			}
			putStmt.put(LOCAL_FILE, localFile);

			final Integer immediateCDStatement = getImmediateCDStatement(cdLines, lineNumber);
			if (immediateCDStatement != null) {
				putStmt.put(REMOTE_DIRECTORY, cdLines.get(immediateCDStatement));
			}
			putStmts.add(putStmt);
		});

		return putStmts;
	}

	private static List<Map<String, String>> getPropertiesForAppendStatement(final Map<Integer, String> cdLines, final List<String> controlCardContent,
			final DiscoveryBuilder.ModuleBuilder stepModule, final StepExec stepExec, final JclContributorContext context) {
		final Map<Integer, String> appendLines = getStatements(controlCardContent, "APPEND ");
		final List<Map<String, String>> appendStmts = new ArrayList<>();
		appendLines.forEach((lineNumber, statement) -> {
			final String[] splitAppendStatement = removeQuotesAndEmptyStrings(statement.split(" "));
			if (splitAppendStatement.length < 2) {
				stepModule.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR, "Invalid APPEND statement: " + statement);
				return;
			}
			final Map<String, String> appendStmt = new HashMap<>();
			appendStmt.put(REMOTE_FILE, MetricsUtility.trimQuotesSpaces(splitAppendStatement[1]));
			String localFile = MetricsUtility.trimQuotesSpaces(splitAppendStatement[0]);
			if (localFile.startsWith(DD_KEYWORD) && localFile.length() > 5) {
				final String ddLocalFile = localFile.substring(5);
				if (stepExec.getDDs().containsKey(ddLocalFile)) {
					localFile = stepExec.getDDs().get(ddLocalFile).stream().map(JobControl::getControlCardName).collect(Collectors.joining(", "));
				}
			} else {
				context.getDiscoveryBuilder().declareExternalModule(localFile, ModuleType.RESOURCE_FILE);
				final ModuleFilter externalModuleFilter = new ModuleFilter().setNames(localFile).setTypes(ModuleType.RESOURCE_FILE);
				final var dependency = stepModule.declareDependency(RelationshipType.ACCESSES, externalModuleFilter, ResolutionFlag.MERGE_DUPLICATES);
				context.getConditionalDependency().ifPresent(x -> x.accept(dependency));
			}
			appendStmt.put(LOCAL_FILE, localFile);

			final Integer immediateCDStatement = getImmediateCDStatement(cdLines, lineNumber);
			if (immediateCDStatement != null) {
				appendStmt.put(REMOTE_DIRECTORY, cdLines.get(immediateCDStatement));
			}
			appendStmts.add(appendStmt);
		});

		return appendStmts;
	}

	private static List<Map<String, String>> getPropertiesForListCatalogStatement(final Map<Integer, String> cdLines, final List<String> controlCardContent,
			final StepExec stepExec) {
		final Map<Integer, String> listCatalogLines = getStatements(controlCardContent, "LS -L");
		final List<Map<String, String>> listCatalogStmts = new ArrayList<>();
		listCatalogLines.forEach((lineNumber, statement) -> {
			final Map<String, String> listCatalogStmt = new HashMap<>();

			final Integer immediateCDStatement = getImmediateCDStatement(cdLines, lineNumber);
			if (immediateCDStatement != null) {
				listCatalogStmt.put(REMOTE_DIRECTORY, cdLines.get(immediateCDStatement));
			}

			if (stepExec.getDDs().containsKey(OUTPUT_DD)) {
				final String ddOutput = stepExec.getDDs().get(OUTPUT_DD).stream().map(JobControl::getControlCardName).collect(Collectors.joining(", "));
				listCatalogStmt.put(LOCAL_FILE, ddOutput);
			}

			listCatalogStmts.add(listCatalogStmt);
		});

		return listCatalogStmts;
	}

	private static String[] removeQuotesAndEmptyStrings(final String[] strings) {
		final String[] result = new String[strings.length];
		for (int i = 0; i < strings.length; i++) {
			if (strings[i].startsWith("\"") || strings[i].startsWith("\'")) {
				result[i] = strings[i].substring(1);
			} else {
				result[i] = strings[i];
			}
			if (strings[i].endsWith("\"") || strings[i].endsWith("\'")) {
				result[i] = strings[i].substring(0, strings[i].length() - 1);
			}
		}
		return Arrays.stream(result).filter(StringUtils::isNotBlank).toArray(String[]::new);
	}

	private static Map<Integer, String> getStatements(final List<String> lines, final String statementType) {
		final Map<Integer, String> statements = new HashMap<>();
		for (int i = 3; i < lines.size(); i++) {
			final String line = StringUtils.trim(lines.get(i));
			if (line.toUpperCase().startsWith(statementType.toUpperCase())) {
				statements.put(i, line.length() == statementType.length() ? "" : StringUtils.trim(line.substring(statementType.length())));
			}
		}
		return statements;
	}

	@Nullable
	private static Integer getImmediateCDStatement(final Map<Integer, String> cdLines, final int lineNumber) {
		for (int i = lineNumber - 1; i >= 0; i--) {
			if (cdLines.containsKey(i)) {
				return i;
			}
		}
		return null;
	}
}
