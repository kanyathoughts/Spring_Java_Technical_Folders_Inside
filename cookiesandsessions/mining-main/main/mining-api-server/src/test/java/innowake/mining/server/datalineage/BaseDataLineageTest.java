/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */

package innowake.mining.server.datalineage;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.server.datalineage.query.DataFlowGraphQueryService;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataInterfaceNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;

public abstract class BaseDataLineageTest extends DatabaseRelatedTest {
	
	protected static final EntityId PROJECT_ID = EntityId.of(1L);
	protected static final String BASE_FOLDER = "./test-resources/innowake/mining/datalineage/source/";
	protected static final Path BASE_EXPECTED_FOLDER = Paths.get("./test-resources/innowake/mining/datalineage/expected/");
	
	/* Change to true if new expected files need to be written */
	protected static final boolean writeExpected = false;
	
	@Autowired
	protected SourceCachingService sourceService;
	@Autowired
	protected ModuleService moduleService;
	@Autowired
	protected DataFlowService dataFlowService;
	@Autowired
	protected DataLineageCoreService dataLineageCoreService;
	
	@Autowired
	protected ExecutorService executorService;
	
	@Autowired
	protected DataFlowGraphQueryService dataFlowGraphQueryService;

	@AfterAll
	public void printProfilingMetrics() {
		if (DefaultProfiler.isProfilingEnabled()) {
			System.out.println("*** Profiling Metrics: ***");
			System.out.println(ProfilingFactory.getProfilingSession().getGlobalMetrics().toString());
		}
	}

	/**
	 * String representation of a list of nodes to check for differences, the related, write and read sets get filled in here. The {@link DataFlowNodePojo} lines
	 * are sorted.
	 * 
	 * @param nodes The list to convert to a String
	 * @return Returns a string representation of the list
	 */
	protected String nodesToString(final List<DataFlowNodePojo> nodes) {
		return nodes.isEmpty() ? "" : nodes.stream()
											.map(this::toOrderedString)
											.sorted()
											.collect(Collectors.joining("\n\r\n"))
										+ "\n\r\n";
	}

	private String toOrderedString(final DataFlowNodePojo pojo) {
		final List<String> relatedSorted = dataFlowService.find(q -> q.withRelationshipFrom(pojo.getId(), DataFlowNodeRelationshipType.RELATED_FIELD)).stream()
				.map(DataFlowNodePojo::getName).sorted().collect(Collectors.toList());
		final List<String> readSorted = dataFlowService.find(q -> q.withRelationshipFrom(pojo.getId(), DataFlowNodeRelationshipType.READ_ACCESS)).stream()
				.map(DataFlowNodePojo::getName).sorted().collect(Collectors.toList());
		final List<String> writeSorted = dataFlowService.find(q -> q.withRelationshipFrom(pojo.getId(), DataFlowNodeRelationshipType.WRITE_ACCESS)).stream()
				.map(DataFlowNodePojo::getName).sorted().collect(Collectors.toList());
		final List<DataFlowErrorPojo> errorsSorted = dataFlowService.findErrors(q -> q.ofNode(pojo.getId())).stream().sorted().collect(Collectors.toList());

		return "DataFlowNode \r\n \t [TYPE = " + pojo.getType() + ",\r\n\t NAME = " + pojo.getName() + ",\r\n\t LOCATION = " + pojo.getLocation().orElse(null) + ",\r\n\t MODULEID = " + pojo.getModuleId().getNid() + ",\r\n\t RELATEDFIELDS = " + relatedSorted + ",\r\n\t READACCESSES = "
				+ readSorted + ",\r\n\t WRITEACCESSES = " + writeSorted + (errorsSorted.isEmpty()? "": ",\r\n\t ERRORS = "+ errorsSorted) +"]";
	}

	/**
	 * Helps formatting all proxycontainers into a String. The {@link ProxyContainerPojo} lines are sorted.
	 *
	 * @param containers Containers to format
	 * @return a formatted string containing all containers
	 */
	protected String containersToString(final List<ProxyContainerPojo> containers) {
		return containers.isEmpty() ? "" : containers.stream()
														.map(this::toOrderedString)
														.sorted()
														.collect(Collectors.joining("\n\r\n"))
											+ "\n\r\n";
	}

	private String toOrderedString(final ProxyContainerPojo pojo) {
		final var fieldNodes = dataFlowService.find(q -> q.byIds(pojo.getFieldNodesUids()));
		return "ProxyContainer [type=" + pojo.getType() + ", moduleId=" + pojo.getModuleNid() + ", properties=" + pojo.getProperties()
				+ ", fields=" + fieldNodes.stream().map(this::toOrderedString).sorted().collect(Collectors.toList())
				+ ", statementLocation=" + pojo.getStatementLocation().orElse(null) + "]";
	}

	/**
	 * Creates a new module and if the technology is UKNOWN, the module is created without source.
	 * @param path Nullable Path of the module
	 * @param moduleName The module name
	 * @param technology The technology of the module
	 * @param type The type of the module 
	 * @return Returns the created module
	 * @throws IOException Thrown if the sourcefile can not be accessed
	 */
	protected EntityId createModule(@Nullable final String path, final String moduleName, final Technology technology, final Type type) throws IOException {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName(moduleName)
				.setProject(PROJECT_ID)
				.setTechnology(technology)
				.setType(type)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY);
		if ( ! technology.equals(Technology.UNKNOWN)) {
			final String content = new String(Files.readAllBytes(Paths.get(BASE_FOLDER).resolve(path)), StandardCharsets.UTF_8).replaceAll("\\r\\n",
					"\n").replaceAll("\\r", "\n");
			final String completePath = BASE_FOLDER + path;

			sourceService.create(new SourcePojoPrototype()
					.setProject(PROJECT_ID)
					.setName(moduleName)
					.setPath(completePath)
					.setTechnology(technology)
					.setType(type)
					.setContent(new BinaryString(content)));

			module.setPath(completePath);
		}
		return moduleService.create(module);
	}
	
	/**
	 * Calls the createModule with an empty path. Used for modules without any sourcefile.
	 * @param moduleName The module name
	 * @param technology The technology of the module
	 * @param type The type of the module 
	 * @return Returns the created module
	 * @throws IOException Thrown if the sourcefile can not be accessed
	 */
	protected EntityId createModule(final String moduleName, final Technology technology, final Type type) throws IOException {
		return moduleService.create(new ModulePojoPrototype()
				.setName(moduleName)
				.setProject(PROJECT_ID)
				.setTechnology(technology)
				.setType(type)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
			);
	}
	
	/**
	 * Creates a new module
	 * @param path Path of the module
	 * @param moduleName The module name
	 * @param technology The technology of the module
	 * @param type The type of the module 
	 * @param projectId The ID of the project the module is created in
	 * @return Returns the created module
	 * @throws IOException Thrown if the sourcefile can not be accessed
	 */
	protected EntityId createModule(final String path, final String moduleName, final Technology technology, final Type type, final EntityId projectId) throws IOException {
		final String content = new String(Files.readAllBytes(Paths.get(BASE_FOLDER).resolve(path)), StandardCharsets.UTF_8).replaceAll("\\r\\n", "\n");
		final String completePath = BASE_FOLDER + path;
		sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName(moduleName)
				.setPath(completePath)
				.setTechnology(technology)
				.setType(type)
				.setContent(new BinaryString(content)));
		return moduleService.create(new ModulePojoPrototype()
				.setName(moduleName)
				.setPath(completePath)
				.setProject(projectId)
				.setTechnology(technology)
				.setType(type)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
			);
	}
	
	protected void compareResults(final String value, final String expectedPath) throws IOException {
		if (writeExpected) {			
			FileUtils.writeStringToFile(BASE_EXPECTED_FOLDER.resolve(expectedPath).toFile(), value, StandardCharsets.UTF_8);
		}
		
		final String expectedNodes = new String(Files.readAllBytes(BASE_EXPECTED_FOLDER.resolve(expectedPath)), StandardCharsets.UTF_8);

		/* unify line terminators and remove leading and trailing space - that makes comparison less error-prone */
		final String valueTrimmed = Arrays.stream(value.split("\n")).map(String::trim).collect(Collectors.joining("\n"));
		final String expectedTrimmed = Arrays.stream(expectedNodes.split("\n")).map(String::trim).collect(Collectors.joining("\n"));

		assertEquals(expectedTrimmed, valueTrimmed);
	}
	
	protected void makeIncludesReference(final EntityId fromId, final EntityId toId) {
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(fromId)
				.setDstModule(toId)
			);
	}

	protected void makeCallReference(final EntityId fromId, final EntityId toId) {
		makeCallReference(fromId, toId, null);
	}

	protected void makeCallReference(final EntityId fromId, final EntityId toId, @Nullable final ModuleLocation fromModuleLocation) {
		final var reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(fromId)
				.setDstModule(toId);
		if (fromModuleLocation != null) {
			reference.setSrcLocation(fromModuleLocation);
		}
		moduleService.createRelationship(reference);
	}
	
	protected void makeReadsWritesReference(final EntityId fromId, final EntityId toId, final ModuleLocation fromModuleLocation, final String dbAccessType) {
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(fromId)
				.setDstModule(toId)
				.setSrcLocation(fromModuleLocation)
				.setProperties(Collections.singletonMap(ModelAttributeKey.DB_ACCESS_TYPE.name(),List.of(dbAccessType)))
			);
	}
	
	protected <T> void assertExpectedConnectionsForIncomingAndOutgoing(final Optional<ListValuedMap<String, List<String>>> expectedConnectionsForImplOutgoing,
			final Optional<ListValuedMap<String, List<String>>> expectedConnectionsForImplIncoming, final Set<? extends DataFlowGraphNode> dataFlowNodes,
			final DataFlowGraph dataFlowGraph) {

		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		for (final DataFlowGraphNode dataFlowGraphNode : dataFlowNodes) {
			final String name = dataFlowGraphNode.getName();
			final List<String> outgoingNodes = dataFlowGraphNode.getOutgoings().stream().map(nodeMap::get).map(DataFlowGraphNode::getName).sorted()
					.collect(Collectors.toList());
			if (expectedConnectionsForImplOutgoing.isPresent() && ! outgoingNodes.isEmpty()) {
				expectedConnectionsForImplOutgoing.get().get(name).replaceAll(nodes ->
						nodes.stream().sorted().collect(Collectors.toList()));
				assertThat(outgoingNodes + " set of field nodes not linked to expected " + expectedConnectionsForImplOutgoing.get().get(name),
						expectedConnectionsForImplOutgoing.get().get(name), hasItems(outgoingNodes));
			}
			final List<String> incomingNodes = dataFlowGraphNode.getIncomings().stream().map(nodeMap::get).map(DataFlowGraphNode::getName).sorted()
					.collect(Collectors.toList());
			if (expectedConnectionsForImplIncoming.isPresent() && ! incomingNodes.isEmpty()) {
				expectedConnectionsForImplIncoming.get().get(name).replaceAll(nodes ->
						nodes.stream().sorted().collect(Collectors.toList()));
				assertThat(incomingNodes + " set of field nodes not linked to expected " + expectedConnectionsForImplIncoming.get().get(name),
						expectedConnectionsForImplIncoming.get().get(name), hasItems(incomingNodes));
			}
		}
	}
	
	protected DataFlowGraph invokeDataFlowGraphQueryService(final EntityId moduleId) {
		return dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(moduleService.getProject(moduleId))
						.setStartModuleIds(Collections.singletonList(moduleId))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());
	}
	
	protected DataFlowGraph invokeDataFlowGraphQueryService(final EntityId moduleId, final ModuleLocation moduleLocation, final DetailLevel detailLevel) {
		return invokeDataFlowGraphQueryService(moduleId, moduleLocation, detailLevel, PROJECT_ID);
	}

	protected DataFlowGraph invokeDataFlowGraphQueryService(final EntityId moduleId, final ModuleLocation moduleLocation, final DetailLevel detailLevel,
			final EntityId projectId) {
		return dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(projectId)
						.setStartModuleIds(Collections.singletonList(moduleId))
						.addStartField(moduleId, moduleLocation.getOffset())
						.setDetailLevel(detailLevel)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());
	}
	
	protected DataFlowGraph invokeDataFlowGraphQueryService(final EntityId moduleId, final ModuleLocation fromModuleLocation) {
		return dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(moduleService.getProject(moduleId))
				.addStartField(moduleId, fromModuleLocation.getOffset())
				.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true)
				.build());
	}
	
	protected DataFlowGraph invokeDataFlowGraphQueryServiceByModule(final EntityId moduleId) {
		return dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(moduleId))
				.setQueryDirection(QueryDirection.BOTH)
				.build());
	}
	
	protected Set<StatementNode> getStatementNodes(final DataFlowGraph dataFlowGraph) {
		return dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
	}
	
	protected Set<FieldNode> getFieldNodes(final DataFlowGraph dataFlowGraph) {
		return dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
	}
	
	protected Set<DataInterfaceNode> getDataInterfaceNodes(final DataFlowGraph dataFlowGraph) {
		return dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE))
				.map(DataInterfaceNode.class::cast).collect(Collectors.toSet());
	}
	
	protected Set<ModuleNode> getModuleNodes(final DataFlowGraph dataFlowGraph) {
		return dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toSet());
	}
	
	protected void makeReadsWritesFileReference(final EntityId fromId, final EntityId toId, final String fileAccessType, final String fileDescriptor, 
			final String fileAlias) {

		final Map<String, Object> properties = new HashMap<>();
		properties.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), fileAccessType);
		properties.put(ModelAttributeKey.COBOL_FD_NAME.name(), fileDescriptor);
		properties.put(ModelAttributeKey.FILE_ALIAS.name(), fileAlias);
		
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(fromId)
				.setDstModule(toId)
				.setProperties(properties));
	}
	
}
