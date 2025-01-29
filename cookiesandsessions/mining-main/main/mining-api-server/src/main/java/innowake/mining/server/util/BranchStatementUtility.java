/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.util;

import static innowake.mining.data.core.api.AstNodeUtils.FIELD_DEFINITION;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.datadictionary.AstNodeToDataDictionaryEntryUtil;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.server.branchstatement.BranchStatement;
import innowake.mining.server.branchstatement.BranchStatementImpl;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Utility Class for Branch Statement Condition extraction.
 */
@Service
public class BranchStatementUtility {

	private static final Logger LOG = LoggerFactory.getLogger(BranchStatementUtility.class);
	private static final String CONDITION_PATH = "conditionPaths";

	@Nullable
	private Tuple2<Long, List<Tuple2<AstNodePojo, DataDictionaryPojo>>> moduleAstAndDDEMap;

	@Autowired
	private MiningDataCoreService dataCoreService;

	@Autowired
	private AstService astService;

	@Autowired
	private AstNodeV2ControlFlowUtil nodeV2ControlFlowUtil;

	private BranchStatementUtility() { }

	/**
	 * Provided the module location of a selected code segment, this method tries to return the list of {@linkplain DataDictionaryPojo}s referenced in
	 * the code segment or an empty list in case it was unsuccessful/no {@linkplain DataDictionaryPojo} was referenced.
	 *
	 * @param moduleId the ID of the module
	 * @param moduleLocation the {@linkplain ModuleLocation}
	 * @return a list of {@linkplain DataDictionaryPojo}s or an empty List
	 */
	public List<DataDictionaryPojo> getReferencedDDEntries(final EntityId moduleId, final ModuleLocation moduleLocation) {
		final long referencedDDEntriesCalculationStartTime =  System.currentTimeMillis();
		LOG.info("Started searching of Referenced DataDictionary for moduleId - {} and moduleLocation(offset - {}, length - {}).",
				moduleId, moduleLocation.getOffset(), moduleLocation.getLength());
		final Long moduleNid = dataCoreService.moduleService.getModuleNid(moduleId);
		final List<AstNodePojo> fieldReferences = astService.find(q -> q.ofModule(moduleId)
				.withSuperTypes("FieldReference")
				.withRetracedOffset(Comperator.GREATER_OR_EQUAL, moduleLocation.getOffset())
				.withRetracedEndOffset(Comperator.LESSER_OR_EQUAL, moduleLocation.getOffset() + moduleLocation.getLength())
				.withIncludedModule(null));

		logFieldReference(moduleId, moduleLocation, fieldReferences);
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> mapOfAstAndDDEs = getMapOfAstAndDDEsForModule(moduleId, moduleNid);
		LOG.info("Count of element in Field Definition and Data Dictionary names map: {}", mapOfAstAndDDEs.size());
		final List<DataDictionaryPojo> referencedDDEntries  =
				getFieldDefinitionFromReferences(moduleId, moduleLocation, fieldReferences, mapOfAstAndDDEs);
		logDataDictionaryEntries(moduleId, moduleLocation, referencedDDEntries);
		final long referencedDDEntriesCalculationEndTime =  System.currentTimeMillis();
		LOG.info("Completed search of Referenced DataDictionary for moduleId - {} and moduleLocation(offset - {}, length - {}) in {} sec",
				moduleId, moduleLocation.getOffset(), moduleLocation.getLength(),
				(referencedDDEntriesCalculationEndTime - referencedDDEntriesCalculationStartTime) / 1000);
		return  referencedDDEntries;
	}

	/**
	 * Provided the module id and module location of a selected code segment of Technology PL1, this method tries to
	 * return the list of {@linkplain DataDictionaryPojo}s referenced in
	 * the code segment or an empty list in case it was unsuccessful/no {@linkplain DataDictionaryPojo} was referenced.
	 *
	 * @param moduleId the module id
	 * @param moduleLocation the {@linkplain ModuleLocation}
	 * @return a list of {@linkplain DataDictionaryPojo}s or an empty List
	 */
	public List<DataDictionaryPojo> getReferencedDDEntriesForPL1(final EntityId moduleId, final ModuleLocation moduleLocation) {
		final Long moduleNid = dataCoreService.moduleService.getModuleNid(moduleId);
		final List<AstNodePojo> fieldReferences = astService.find(q -> q.ofModule(moduleId)
				.withSuperTypes("FieldReference")
				.withRetracedOffset(Comperator.GREATER_OR_EQUAL, moduleLocation.getOffset())
				.withRetracedEndOffset(Comperator.LESSER_OR_EQUAL, moduleLocation.getOffset() + moduleLocation.getLength()));
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> mapOfAstAndDDEs = getMapOfAstAndDDEsForModule(moduleId, moduleNid);
		return getFieldDefinitionFromReferences(moduleId, moduleLocation, fieldReferences, mapOfAstAndDDEs);
	}

	/**
	 * Provide the conditions of the branch statement used to reach the piece of code marked by moduleLocation.
	 *
	 * @param moduleId moduleId of the module where the piece of code is present.
	 * @param moduleLocation of the piece of code.
	 * @return List of tuple binding {@linkplain BranchStatement} and Condition of statement
	 */
	public List<Tuple2<BranchStatement, String>> getConditions(final EntityId moduleId, final ModuleLocation moduleLocation) {
		final long conditionsCalculationStartTime =  System.currentTimeMillis();
		LOG.info("Started fetching of branch conditions for moduleId - {} and moduleLocation(offset - {}, length - {}).",
				moduleId, moduleLocation.getOffset(), moduleLocation.getLength());
		if (astService.findModuleRelationships(q -> q.ofModule(moduleId).withType(AstModuleRelationshipType.ENTRY)).isEmpty()) {
			LOG.info("Control flow graph is not present for module ID - {}. Calculating it.", moduleId);
			boolean cfgCalculated = innowake.mining.data.core.controlflow.ControlFlowGraph.calculate(moduleId, new DefaultStoreAstExecutor(), dataCoreService);
			if (cfgCalculated) {
				LOG.info("Control flow graph is successfully calculated for module ID - {}", moduleId);
			} else {
				LOG.error("Failed to calculate Control flow graph for module ID - {} and Module Location: (offset - {}, length - {}).",
						moduleId, moduleLocation.getOffset(), moduleLocation.getLength());
				throw new IllegalStateException("Failed to calculate Control flow graph for module ID - " + moduleId +
						" and Module Location: (offset - " + moduleLocation.getOffset() + ", length - " + moduleLocation.getLength() + ").");
			}
		}
		final long utilityCalculationStartTime = System.currentTimeMillis();
		LOG.info("Searching branch Conditions using Utility class AstNodeV2ControlFlowUtil for moduleId - {}, moduleLocation(offset - {}, length - {}.",
				moduleId, moduleLocation.getOffset(), moduleLocation.getLength());
		final List<Tuple2<AstNodePojo, String>> list = nodeV2ControlFlowUtil.findConditions(moduleId, moduleLocation);
		final long utilityCalculationEndTime = System.currentTimeMillis();
		LOG.info("Completed the search of branch Conditions using Utility class AstNodeV2ControlFlowUtil for moduleId - {},"
						+ "moduleLocation(offset - {}, length - {} in {} sec.", moduleId, moduleLocation.getOffset(), moduleLocation.getLength(),
						(utilityCalculationEndTime - utilityCalculationStartTime) / 1000);
		final Long moduleNid = dataCoreService.moduleService.getModuleNid(moduleId);
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> mapOfAstAndDDEs = getMapOfAstAndDDEsForModule(moduleId, moduleNid);
		final List<Tuple2<BranchStatement, String>> branchStatements = new ArrayList<>();
		for (final Tuple2<AstNodePojo, String> tuple2 : list) {
			final AstNodePojo astNodeV2 = tuple2.a;
			final List<DataDictionaryPojo> dataDictionaryEntries = getDataDictionariesForCondition(moduleId, moduleLocation, astNodeV2, mapOfAstAndDDEs);
			final Tuple2<BranchStatement, String> branchStatement = new Tuple2<>(new BranchStatementImpl(astNodeV2, dataDictionaryEntries), tuple2.b);
			branchStatements.add(branchStatement);
		}
		final long conditionsCalculationEndTime = System.currentTimeMillis();
		LOG.info("Completed fetching of branch condition for moduleId - {}, moduleLocation(offset - {}, length - {} in {} Sec.", moduleId,
				moduleLocation.getOffset(), moduleLocation.getLength(), (conditionsCalculationEndTime - conditionsCalculationStartTime) / 1000);
		return branchStatements;
	}

	private List<DataDictionaryPojo> getDataDictionariesForCondition(final EntityId moduleId, final ModuleLocation moduleLocation, final AstNodePojo statement,
			final List<Tuple2<AstNodePojo, DataDictionaryPojo>> mapOfAstAndDDEs) {
		final List<DataDictionaryPojo> dataDictionaryEntries = new ArrayList<>();
		final Map<String, Object> properties = statement.getProperties();
		if (properties.containsKey(CONDITION_PATH)) {
			final List<AstNodePojo> conditionsNodes = getConditionNodes((String) properties.get(CONDITION_PATH), statement);
			for (final AstNodePojo conditionNode : conditionsNodes) {
				final AstNodeLocation astNodeLocation = conditionNode.getLocation();
				if (astNodeLocation != null) {
					Optional<Integer> offset = astNodeLocation.getRetracedOffset();
					Optional<Integer> length = astNodeLocation.getRetracedLength();
					if (offset.isPresent() && length.isPresent()) {
						final List<AstNodePojo> conditionalDDEntriesReference = astService.find(q -> q.ofModule(moduleId)
								.withSuperTypes("FieldReference")
								.withRetracedOffset(Comperator.GREATER_OR_EQUAL, offset.get())
								.withRetracedEndOffset(Comperator.LESSER_OR_EQUAL, offset.get() + length.get())
								.withIncludedModule(null));

						dataDictionaryEntries.addAll(getFieldDefinitionFromReferences(moduleId, moduleLocation, conditionalDDEntriesReference, mapOfAstAndDDEs));
					} else {
						LOG.error("Offset and Length is null for conditionNode: {}", conditionNode);
					}
				} else {
					LOG.error("Advanced module location is null for conditionNode: {}", conditionNode);
				}
			}
		}
		return dataDictionaryEntries;
	}

	private List<DataDictionaryPojo> getFieldDefinitionFromReferences(final EntityId moduleId, final ModuleLocation moduleLocation,
																	  final List<AstNodePojo> fieldReferences, final List<Tuple2<AstNodePojo, DataDictionaryPojo>>  mapOfAstAndDDEs) {
		if ( ! fieldReferences.isEmpty()) {
			final Set<AstNodePojo> fieldDefinitions = fieldReferences.stream().map(AstNodeUtils :: getFieldDefinitionFromReference)
					.filter(def -> def.getSuperTypes().contains(FIELD_DEFINITION)).collect(Collectors.toSet());
			logFieldDefinition(moduleId, moduleLocation, fieldDefinitions);
			return mapOfAstAndDDEs.stream()
					.filter(t -> fieldDefinitions.stream().anyMatch(n -> n.getLabel().equals(t.a.getLabel()) &&
							n.getLocation().getRetracedOffset().equals(t.a.getLocation().getRetracedOffset()) &&
							n.getLocation().getRetracedLength().equals(t.a.getLocation().getRetracedLength()) &&
							t.b.getName().equals(n.getProperties().get("name")) &&
							( n.getIncludedModule().isEmpty()
									|| ( n.getIncludedModule().isPresent() && t.a.getIncludedModule().isPresent()
										&& n.getIncludedModule().get().equals(t.a.getIncludedModule().get())))))
					.map(t -> t.b)
					.collect(Collectors.toList());
		}
		return Collections.emptyList();
	}

	private List<AstNodePojo> getConditionNodes(final String conditionPath, final AstNodePojo statement) {
		final List<AstNodePojo> conditionsNodes = new ArrayList<>();
		final Gson gson = new Gson();
		final int[][] conditionPaths = gson.fromJson(conditionPath, int[][].class);
		for (final int[] path : conditionPaths) {
			AstNodePojo currentNode = statement;
			if (currentNode != null) {
				for (final int index : path) {
					final List<AstNodePojo> children = currentNode.getChildren();
					if (index >= 0 && index < children.size()) {
						currentNode = children.get(index);
					} else {
						currentNode = null;
						break;
					}
				}
				conditionsNodes.add(currentNode);
			}
		}
		return conditionsNodes;
	}

	private List<Tuple2<AstNodePojo, DataDictionaryPojo>> getMapOfAstAndDDEsForModule(final EntityId moduleId, final Long moduleNid) {
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> mapOfAstAndDDEs;
		if (moduleAstAndDDEMap != null && Objects.requireNonNull(moduleAstAndDDEMap).a.equals(moduleNid)) {
			mapOfAstAndDDEs = Objects.requireNonNull(moduleAstAndDDEMap).b;
		} else {
			final AstNodeToDataDictionaryEntryUtil dao = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), dataCoreService);
			mapOfAstAndDDEs = dao.findConnections(moduleId);
			moduleAstAndDDEMap = new Tuple2<>(moduleNid, mapOfAstAndDDEs);
		}
		return mapOfAstAndDDEs;
	}

	private void logFieldReference(final EntityId moduleId, final ModuleLocation moduleLocation, final List<AstNodePojo> fieldReferences) {
		try {
			List<Object> unresolvedFieldNames = fieldReferences.stream()
					.map(entry -> entry.getProperties().get("unresolvedFieldName"))
					.collect(Collectors.toList());
			LOG.debug(() -> String.format("Field Referenced found for moduleId - %s and moduleLocation(offset - %d, length - %d) - Count - %d Variable names: "
							+ "%s",
					moduleId, moduleLocation.getOffset(), moduleLocation.getLength(), fieldReferences.size(),
					unresolvedFieldNames.stream()
							.map(Object::toString)
							.collect(Collectors.joining(", "))));
		} catch (Exception ex){
			LOG.error("Error Occurred while finding unresolvedFieldNames: {}", ex.getMessage(), ex);
		}
	}

	private void logFieldDefinition(final EntityId moduleId, final ModuleLocation moduleLocation, final Set<AstNodePojo> fieldDefinitions) {
		try {
			List<Object> fieldDefinitionElementNames = fieldDefinitions.stream()
					.map(entry -> entry.getProperties().get("name"))
					.collect(Collectors.toList());
			LOG.debug(() -> String.format("Field Definition found for moduleId - %s and moduleLocation(offset - %d, length - %d) - Count - %d Variable names:"
							+ " %s", moduleId, moduleLocation.getOffset(), moduleLocation.getLength(), fieldDefinitions.size(),
					fieldDefinitionElementNames.stream()
							.map(Object::toString)
							.collect(Collectors.joining(", "))));
		} catch (Exception ex){
			LOG.error("Error Occurred while logging fieldDefinition: {}", ex.getMessage(), ex);
		}
	}

	private void logDataDictionaryEntries(final EntityId moduleId, final ModuleLocation moduleLocation, final List<DataDictionaryPojo> referencedDDEntries) {
		LOG.debug(() -> String.format("Found %d Referenced DataDictionary for moduleId - %s and moduleLocation(offset - %d, length - %d): DataElementNames: %s",
				referencedDDEntries.size(),
				moduleId, moduleLocation.getOffset(), moduleLocation.getLength(),
				referencedDDEntries.stream()
						.map(DataDictionaryPojo::getName)
						.collect(Collectors.joining(", "))));
	}
}
