
/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.server.discovery.feature.matrix.FeatureMatrix;
import innowake.mining.server.discovery.feature.matrix.FeatureMatrixRoot;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.RelationshipType;

/**
 * Validates a given module using the feature matrix. It accumulates the identified mismatches between feature matrix and the module properties.
 * When the validation fails, it's not always an error with the module properties.
 * It's possible that a feature might be introduced but the feature matrix might have not updated in which case it requires an update.
 */
public class DiscoveryFeatureValidator {
	private final ModulePojo module;
	private final List<Message> messages = new ArrayList<>();
	private static final String FILE_NAME = "./src/main/resources/discoveryFeatureMatrix.json";
	private static final String PHY_LINES_COUNT = "Physical Lines of Code";
	private static final String CODE_LINES_COUNT = "Source Lines of Code";
	private static final String COMMENT_LINES_COUNT = "Comment Lines of Code";
	private static final String DEAD_CODE_LINES_COUNT = "Dead code lines count";
	private static final String COMPLEXITY_MCCABE = "Complexity McCabe";
	private static final String SQL_STATEMENTS = "Sql Statements";
	private static final String STATEMENTS = "Statements";
	private static Map<String, FeatureMatrix> FEATURE_MATRICES = new HashMap<>();
	private static final String MATRIX_OUTDATED_MESSAGE = "Looks this feature is new. Update feature matrix json!";
	@Nullable
	private final FeatureMatrix featureMatrix;
	@Nullable
	private final SourceMetricsPojo sourceMetrics;
	private final ModuleService moduleService;
	private final Map<String, Set<RelationshipType>> technologyTypeRelationships;

	static {
		final ObjectMapper mapper = new ObjectMapper();
		final FeatureMatrixRoot featureMatrixRoot;
		try {
			featureMatrixRoot = mapper.readValue(new FileReader(FILE_NAME), FeatureMatrixRoot.class);
			FEATURE_MATRICES = featureMatrixRoot.getFeatureMatrices().stream()
					.collect(Collectors.toMap(feature -> feature.getTechnology() + "_" + feature.getType(), feature -> feature));
		} catch (final IOException e) {
			throw new IllegalStateException("Could not load discovery feature matrix file", e);
		}
	}

	public DiscoveryFeatureValidator(final ModulePojo module, final ModuleService moduleService) {
		this.module = module;
		this.moduleService = moduleService;
		sourceMetrics = module.getSourceMetrics().orElse(null);
		featureMatrix = FEATURE_MATRICES.get(getTechnologyType(module));
		if (featureMatrix != null) {
			technologyTypeRelationships = featureMatrix.getDependencies().stream().collect(Collectors.toMap(x -> x.getModuleType(), x -> x.getRelationship()));
		} else {
			technologyTypeRelationships = Collections.emptyMap();
		}
	}

	private static String getTechnologyType(final ModulePojo module) {
		return module.getTechnology() + "_" + module.getType();
	}

	/**
	 * Performs the validation.
	 *
	 * @return a list of messages. If it's empty then there is no error during validation.
	 */
	public List<Message> validate() {
		if (featureMatrix == null) {
			addMessage(new Message("Technology & type combination is not present in json", MATRIX_OUTDATED_MESSAGE));
		} else if (module.getIdentification() == Identification.IDENTIFIED) {
			validateLinesOfCode();
			validatePhysicalLinesCode();
			validateCommentedLinesCode();
			validateDeadCodeLines();
			validateComplexityMcCabe();
			validateStatement();
			validateSqlStatement();
			validateNature();
			validateDependency();
			validateContains();
		} else {
			validateExternalModuleNature(); /* For external modules, the module#identification might not be identified. so we need to validate here */
		}
		return messages;
	}

	private void validateNature() {
		final Representation representation = module.getRepresentation().orElse(null);
		final var featureMatrixRepresentation = assertNotNull(featureMatrix).getRepresentation();
		if (featureMatrixRepresentation.contains(ContributorResult.Type.ROOT_MODULE.name())) {
			if (Representation.PHYSICAL != representation) {
				addMessage(new Message(Representation.PHYSICAL.name(), representation == null ? "does not exists" : representation.toString(), "representation"));
			}
			if (StringUtils.isBlank(module.getPath().orElse(null))) {
				addMessage(new Message("Path should be present but empty."));
			}
		}
		if (featureMatrixRepresentation.size() == 1 && featureMatrixRepresentation.contains(ContributorResult.Type.SUB_MODULE.name())) {
			if (Representation.VIRTUAL != representation) {
				addMessage(new Message(Representation.VIRTUAL.name(), representation == null ? "does not exists" : representation.toString(), "representation"));
			}
			if (StringUtils.isBlank(module.getParentPath().orElse(null))) {
				addMessage(new Message("\"Contains path\" should be present but empty."));
			}
		}
		validateExternalModuleNature();
	}

	private void validateExternalModuleNature() {
		final var representation = assertNotNull(featureMatrix).getRepresentation();		
		if (representation.size() == 1 && representation.contains(ContributorResult.Type.EXTERNAL_MODULE.name())) {
			if (StringUtils.isNotBlank(module.getPath().orElse(null))) {
				addMessage(new Message("Path should be empty but present."));
			}
			if (StringUtils.isNotBlank(module.getParentPath().orElse(null))) {
				addMessage(new Message("\"Contains path\" should be empty but present."));
			}
		}
	}

	private void validateDependency() {
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(module.identity())
																							.withTypes(RelationshipType.DEPENDENCY_TYPES)
																							.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION)
																							.includeModuleDetails(true, true));
		for (final var reference : references) {
			final Optional<ModulePojo> referencedModule = moduleService.findAnyModule(q -> q.byUid(reference.getDstModule()));
			/* in case the module is not found in the project then it would be in project 0 which is a utility invocation and we ignore it */
			if (referencedModule.isPresent()) {
				final RelationshipType moduleRelationship = reference.getRelationship();
				final Set<RelationshipType> relationships = technologyTypeRelationships.get(getTechnologyType(referencedModule.get()));
				if (relationships == null || ! relationships.contains(moduleRelationship)) {
					addMessage(new Message(
							String.format("Invalid dependency: id - %s, name - %s, technology - %s, type - %s, relationship - %s", referencedModule.get().getId(),
									referencedModule.get().getName(), referencedModule.get().getTechnology(), referencedModule.get().getType(), moduleRelationship),
							MATRIX_OUTDATED_MESSAGE));
				}
			}
		}
	}

	private void validateContains() {
		final List<ModulePojo> modules = moduleService.findModules(q -> q.withSourceRelationshipsFrom(assertNotNull(module.identity()), RelationshipType.CONTAINS));
		modules.stream()
			.filter(module -> ! assertNotNull(featureMatrix).getContains().contains(getTechnologyType(module)))
			.forEach(module -> {
				addMessage(new Message(String.format("Invalid contains module: id - %s, name - %s, path - %s, technology - %s, type - %s", module.getId(),
						module.getName(), module.getPath(), module.getTechnology(), module.getType()), MATRIX_OUTDATED_MESSAGE));
			});
	}

	private void addMessage(final Message message) {
		messages.add(message);
	}

	private void validateLinesOfCode() {
		final boolean isSupported = assertNotNull(featureMatrix).getFullySupportedFeatures().contains(CODE_LINES_COUNT);

		if (isSupported && sourceMetrics == null) {
			addMessage(new Message("Lines of code", "positive or zero", "does not exists"));
			return;
		}
		if (isSupported) {
			final Integer codeLines = assertNotNull(sourceMetrics).getCodeLines();
			if (codeLines == null || codeLines.intValue() < 0) {
				addMessage(new Message("Lines of code", "positive or zero", codeLines == null ? "does not exists" : codeLines.toString()));
			}
		} else if (sourceMetrics != null) {
			final Integer codeLines = sourceMetrics.getCodeLines();
			if (codeLines != null && codeLines.intValue() > 0) {
				addMessage(new Message("Lines of code", "negative or zero", codeLines.toString(), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	private void validatePhysicalLinesCode() {
		final boolean isSupported = assertNotNull(featureMatrix).getFullySupportedFeatures().contains(PHY_LINES_COUNT);

		if (isSupported && sourceMetrics == null) {
			addMessage(new Message("Physical lines", "positive or zero", "does not exists"));
			return;
		}
		if (isSupported) {
			final Integer physicalLines = assertNotNull(sourceMetrics).getPhysicalLines();
			if (physicalLines == null || physicalLines.intValue() < 0) {
				addMessage(new Message("Physical lines", "positive or zero", physicalLines == null ? "does not exists" : physicalLines.toString()));
			}
		} else if (sourceMetrics != null) {
			final Integer physicalLines = sourceMetrics.getPhysicalLines();
			if (physicalLines != null && physicalLines.intValue() > 0) {
				addMessage(new Message("Physical lines", "negative or zero", physicalLines.toString(), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	private void validateCommentedLinesCode() {
		final boolean isSupported = assertNotNull(featureMatrix).getFullySupportedFeatures().contains(COMMENT_LINES_COUNT);

		if (isSupported && sourceMetrics == null) {
			addMessage(new Message("Commented lines", "positive or zero", "does not exists"));
			return;
		}
		if (isSupported) {
			final Integer commentLines = assertNotNull(sourceMetrics).getCommentLines();
			if (commentLines == null || commentLines.intValue() < 0) {
				addMessage(new Message("Commented lines", "positive or zero", commentLines == null ? "does not exists" : commentLines.toString()));
			}
		} else if (sourceMetrics != null) {
			final Integer commentLines = sourceMetrics.getCommentLines();
			if (commentLines != null && commentLines.intValue() > 0) {
				addMessage(new Message("Commented lines", "negative or zero", commentLines.toString(), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	private void validateDeadCodeLines() {
		final boolean isSupported = assertNotNull(featureMatrix).getFullySupportedFeatures().contains(DEAD_CODE_LINES_COUNT);

		if (isSupported && sourceMetrics == null) {
			addMessage(new Message("Dead code lines", "positive or zero", "does not exists"));
			return;
		}
		if (isSupported) {
			final Integer deadCodeLines = assertNotNull(sourceMetrics).getDeadCodeLines();
			if (deadCodeLines == null) {
				addMessage(new Message("Dead code lines", "positive or zero", "null"));
			} else if (deadCodeLines.intValue() < 0) {
				addMessage(new Message("Dead code lines", "positive or zero", deadCodeLines.toString()));
			}
		} else if (sourceMetrics != null) {
			final Integer deadCodeLines = sourceMetrics.getDeadCodeLines();
			if (deadCodeLines == null) {
				addMessage(new Message("Dead code lines", "positive or zero", "null"));
			} else if (deadCodeLines.intValue() > 0) {
				addMessage(new Message("Dead code lines", "negative or zero", deadCodeLines.toString(), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	private void validateComplexityMcCabe() {
		final boolean isSupported = assertNotNull(featureMatrix).getFullySupportedFeatures().contains(COMPLEXITY_MCCABE);

		if (isSupported && sourceMetrics == null) {
			addMessage(new Message("Complexity McCabe", "positive or zero", "does not exists"));
			return;
		}
		if (isSupported) {
			final Integer complexity = assertNotNull(sourceMetrics).getComplexityMcCabe();
			if (complexity == null || complexity.intValue() < 0) {
				addMessage(new Message("Complexity McCabe", "positive or zero", complexity == null ? "does not exists" : complexity.toString()));
			}
		} else if (sourceMetrics != null) {
			final Integer complexity = sourceMetrics.getComplexityMcCabe();
			if (complexity != null && complexity.intValue() > 0) {
				addMessage(new Message("Complexity McCabe", "negative or zero", complexity.toString(), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	private void validateStatement() {
		final int statementsCount = module.getStatements();
		if (assertNotNull(featureMatrix).getFullySupportedFeatures().contains(STATEMENTS)) {
			if (statementsCount < 0) {
				addMessage(new Message("Statements count", "positive or zero", Integer.toString(statementsCount)));
			}
		} else {
			if (statementsCount > 0) {
				addMessage(new Message("Statements count", "negative or zero", Integer.toString(statementsCount), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	private void validateSqlStatement() {
		final int sqlStatementsCount = module.getSqlStatements();
		if (assertNotNull(featureMatrix).getFullySupportedFeatures().contains(SQL_STATEMENTS)) {
			if (sqlStatementsCount < 0) {
				addMessage(new Message("Sql Statements count", "positive or zero", Integer.toString(sqlStatementsCount)));
			}
		} else {
			if (sqlStatementsCount > 0) {
				addMessage(new Message("Sql Statements count", "negative or zero", Integer.toString(sqlStatementsCount), MATRIX_OUTDATED_MESSAGE));
			}
		}
	}

	/**
	 * Class to format the validation message.
	 */
	class Message {
		private final String value;

		private Message(String message) {
			value = String.format("%s. Module: id - %s, name - %s, path - %s, technology - %s, type - %s.", message, module.getId(), module.getName(),
					module.getPath(), module.getTechnology(), module.getType());
		}

		private Message(String message, String additionalMessage) {
			value = String.format("%s. Module: id - %s, name - %s, path - %s, technology - %s, type - %s. %s", message, module.getId(), module.getName(),
					module.getPath(), module.getTechnology(), module.getType(), additionalMessage);
		}

		private Message(String assertion, String expected, String actual) {
			value = String.format("Expected: %s but: %s for %s. Module: id - %s, name - %s, path - %s, technology - %s, type - %s.", expected, actual,
					assertion, module.getId(), module.getName(), module.getPath(), module.getTechnology(), module.getType());
		}

		private Message(String assertion, String expected, String actual, String additionalMessage) {
			value = String.format("Expected: %s but: %s for %s. Module: id - %s, name - %s, path - %s, technology - %s, type - %s. %s", expected, actual,
					assertion, module.getId(), module.getName(), module.getPath(), module.getTechnology(), module.getType(), additionalMessage);
		}

		public String value() {
			return value;
		}
	}

}
