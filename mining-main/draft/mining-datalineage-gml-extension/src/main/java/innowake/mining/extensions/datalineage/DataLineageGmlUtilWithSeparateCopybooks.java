/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.datalineage;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.systemdir.gml.YedGmlWriter;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.SourceLocation;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode.Type;
import innowake.mining.shared.model.datalineage.graph.DataInterfaceNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.datalineage.util.Copybook;
import innowake.mining.shared.model.datalineage.util.DataInterface;
import innowake.mining.shared.model.datalineage.util.DataLineageGraphicsProvider;
import innowake.mining.shared.model.datalineage.util.DataLineageGraphicsProviderForSeparateCopybooks;
import innowake.mining.shared.model.datalineage.util.Element;
import innowake.mining.shared.model.datalineage.util.Field;
import innowake.mining.shared.model.datalineage.util.Module;
import innowake.mining.shared.model.datalineage.util.ProxyContainer;
import innowake.mining.shared.model.datalineage.util.Statement;
import innowake.mining.shared.model.datalineage.util.YedGmlWriterWithNesting;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * Read input from JSON file and generate a .gml-file to visualize data flow in
 * yEd with nesting of modules.
 */
public class DataLineageGmlUtilWithSeparateCopybooks {

    private final DataFlowGraph dataFlowGraph;
    private final List<Module> moduleList = new ArrayList<>();
    private final List<DataInterface> dataInterfaceList = new ArrayList<>();
    private final List<Field> fieldList = new ArrayList<>();
    private final List<Statement> statementList = new ArrayList<>();
    private final Set<String> allIds = new HashSet<>();
    private final Map<String, Element> elementsByIdMap = new HashMap<>();
    private final Map<String, Set<Element>> moduleFieldsStatements = new HashMap<>();
    
    private final Map<Module, Set<Element>> collapsableGroups = new HashMap<>();

    private final Map<Long, Copybook> copybooksByModuleId = new HashMap<>();
    private final Set<String> copybookElements = new HashSet<>();
    private final Map<String, Set<String>> moduleProxyContainerNames = new HashMap<>();
    /*
     * One map to indicate proxy containers per module.
     * Another map to indicate data interfaces per proxy container.
     * Should be refactored into one single map.
     */
    private final Map<String, Set<ProxyContainer>> moduleProxyContainers = new HashMap<>();
    private final Map<String, Set<Element>> proxyContainerDataInterfaces = new HashMap<>();

    private final SimpleDirectedGraph<Element, DefaultEdge> graph = new SimpleDirectedGraph<>(DefaultEdge.class);
    private final Set<DefaultEdge> biEdges = new HashSet<>();

    /**
     * 
     * @param json file that contains data lineage model
     * @throws IOException Invalid paths for input and output or the wrong
     *                     UTF-format of the JSON-file can cause exceptions. Since
     *                     this method will be removed later anyway, we can leave it
     *                     without handling at this point.
     */
    public DataLineageGmlUtilWithSeparateCopybooks(final String json) throws IOException {
    	final ObjectMapper mapper = new ObjectMapper();
        final ResultContainer container = mapper.readValue(json, ResultContainer.class);
        final DataFlowGraph dfGraph = (DataFlowGraph) container.getObject();
        this.dataFlowGraph = dfGraph;
    }

    private void addModules(final List<DataFlowGraphNode> moduleNodes) {
    	for (final DataFlowGraphNode node : moduleNodes) {
    		final Module module = new Module(node);
    		addElement(node, module);
    		moduleList.add(module);
            final String id = node.getId();
            moduleFieldsStatements.put(id, new HashSet<>());
            moduleProxyContainers.put(id, new HashSet<>());
            moduleProxyContainerNames.put(id, new HashSet<>());
    	}
    }
    
    private void addElement(final DataFlowGraphNode node, final Element element) {
    	final String id = node.getId();
    	allIds.add(id);
    	graph.addVertex(element);
    	elementsByIdMap.put(id, element);
    }
    
    private void addCopybookInformation(final DataFlowGraphNode node) {
    	final SourceLocation sourceLocation = node.getSourceLocation();
    	if (sourceLocation == null) {
    		return;
    	}
    	final long copybookModuleId = sourceLocation.getModuleId();
    	if ( ! copybooksByModuleId.containsKey(copybookModuleId)) {
    		final String copybookId = nextID(allIds);
    		final ModuleNode tmp = new ModuleNode(copybookId, copybookModuleId, sourceLocation.getModuleName(),
    				sourceLocation.getModuleLocation());
    		final Copybook copybook = new Copybook(tmp);
    		graph.addVertex(copybook);
    		elementsByIdMap.put(copybookId, copybook);
    		copybooksByModuleId.put(copybookModuleId, copybook);
    	}
    	copybookElements.add(node.getId());
    }
    
    /**
     * Iterate through all data flow graph nodes and add them to the graph. Modules need to be handled first and 
     * differently to assure that other elements like statements can be added to their modules accordingly.
     * 
     * If a node turns out to be a copybook, we need to assess more information and assign a new ID with that node.
     */
    private void organizeNodes() {
        final List<DataFlowGraphNode> nodes = dataFlowGraph.getNodes();
        addModules(nodes.stream().filter(v -> v.getType() == Type.MODULE).collect(Collectors.toList()));

        for (final DataFlowGraphNode node : nodes) {
            final Type type = node.getType();
            switch (type) {
                case MODULE:
                    break;

                case DATA_INTERFACE:
                    final DataInterface dataInterface = new DataInterface(node);
                    addElement(node, dataInterface);
                    dataInterfaceList.add(dataInterface);
                    break;

                case FIELD:
                    final Field field = new Field(node);
                    addElement(node, field);
                    fieldList.add(field);
                    break;

                case STATEMENT:
                    final Statement statement = new Statement(node);
                    addElement(node, statement);
                    statementList.add(statement);
                    break;
            }
            if (isFromCopybook(node)) {
                addCopybookInformation(node);
            }
        }
    }

    /**
     * Creates a GML-file representing the data flow graph for Yed.
     *
     * @return Generated GML-file.
     */
    public String getGml() {
    	organizeNodes();
    	assignDataInterfaces();
        assignElements(statementList, moduleFieldsStatements);
        assignElements(fieldList, moduleFieldsStatements);
        assignElementsToGroups();
        setEdges();
        return exportGraphAsString();
    }
    
    private void assignDataInterfaces() {
    	for (final DataInterface dataInterface : dataInterfaceList) {
    		final DataFlowGraphNode node = dataInterface.getFlowGraphNode();
    		final String parentModuleId = node.getParentModule();
    		if (parentModuleId == null) {
    			continue;
    		}
    		final String proxyContainerName = getProxyContainerName(node.getId());
    		ProxyContainer proxyContainer = findProxyContainer(parentModuleId, proxyContainerName);
    		if (proxyContainer != null) {
    			proxyContainerDataInterfaces.get(proxyContainer.getFlowGraphNode().getId()).add(dataInterface);
    		} else {
    			final DataFlowGraphNode tmp = new DataInterfaceNode(nextID(allIds), proxyContainerName,
    					new ModuleLocation());
    			proxyContainer = new ProxyContainer(tmp);
    			moduleProxyContainerNames.get(parentModuleId).add(proxyContainerName);
    			moduleProxyContainers.get(parentModuleId).add(proxyContainer);
    			proxyContainerDataInterfaces.put(tmp.getId(), new HashSet<>());
    			proxyContainerDataInterfaces.get(tmp.getId()).add(dataInterface);
    		}
    	}
    }
    
    private static String getProxyContainerName(final String dataInterfaceId) {
    	final Pattern pattern = Pattern.compile(".*container-(.*)-interface.*");
    	final Matcher matcher = pattern.matcher(dataInterfaceId);
    	matcher.matches();
    	return matcher.group(1);
    }
    
    @Nullable
    private ProxyContainer findProxyContainer(final String parentModuleId, final String proxyContainerName) {
    	if (moduleProxyContainerNames.get(parentModuleId).contains(proxyContainerName)) {
			for (final ProxyContainer proxyContainer : moduleProxyContainers.get(parentModuleId)) {
				if (proxyContainer.getFlowGraphNode().getName().equals(proxyContainerName)) {
					return proxyContainer;
				}
			}
    	}
    	return null;
    }

    private static void assignElements(final List<? extends Element> elements, final Map<String, Set<Element>> toBeAdded) {
        for (final Element element : elements) {
            final String parentModuleId = element.getFlowGraphNode().getParentModule();
            if (parentModuleId != null) {
            	toBeAdded.get(parentModuleId).add(element);
            }
        }
    }
    
    private void assignElementsToGroups() {
    	for (final Module module : moduleList) {
    		final String moduleId = module.getFlowGraphNode().getId();
    		final Set<ProxyContainer> proxyContainers = moduleProxyContainers.get(moduleId);
    		final Set<Element> moduleElements = moduleFieldsStatements.get(moduleId);
    		if ( ! proxyContainers.isEmpty()) {
    			for (final ProxyContainer container : proxyContainers) {
    				moduleElements.add(container);
					collapsableGroups.put(container, proxyContainerDataInterfaces.get(container.getFlowGraphNode().getId()));
    			}
    		}
    		collapsableGroups.put(module, moduleElements);
    	}
    }

    private static String nextID(final Set<String> ids) {
        String nextCandidate = UUID.randomUUID().toString();
        while (ids.contains(nextCandidate)) {
            nextCandidate = UUID.randomUUID().toString();
        }
        ids.add(nextCandidate);
        return nextCandidate;
    }

    private static boolean isFromCopybook(final DataFlowGraphNode node) {
        final SourceLocation sourceLocation = node.getSourceLocation();
        if (sourceLocation == null) {
            return false;
        }
        return ! ("module-" + sourceLocation.getModuleId()).equals(node.getParentModule());
    }

	private void setEdges() {
		for (final DataFlowGraphNode node : dataFlowGraph.getNodes()) {
			final Element source = elementsByIdMap.get(node.getId());
			handleEdge(node, source);
			checkCopybookEdge(node, source);
		}
	}

	private void handleEdge(final DataFlowGraphNode node, final Element source) {
		for (final String id : node.getOutgoings()) {
			if (id.equals(node.getId())) {
				continue;
			}
			final Element target = elementsByIdMap.get(id);
			if (target != null) {
				final DefaultEdge reverseEdge = graph.getEdge(target, source);
				if (reverseEdge != null) {
					biEdges.add(reverseEdge);
				} else {
					graph.addEdge(source, target, new DefaultEdge());
				}
			}
		}
	}

	private void checkCopybookEdge(final DataFlowGraphNode node, final Element source) {
		if (copybookElements.contains(node.getId())) {
			final SourceLocation sourceLocation = node.getSourceLocation();
			if (sourceLocation != null) {
				final Copybook copybook = copybooksByModuleId.get(sourceLocation.getModuleId());
				graph.addEdge(copybook, source, new DefaultEdge());
			}
		}
	}

    private String exportGraphAsString() {
        final DataLineageGraphicsProvider graphicsProvider = new DataLineageGraphicsProviderForSeparateCopybooks(biEdges);
        final YedGmlWriterWithNesting<Element, DefaultEdge, Module> gmlWriter = new YedGmlWriterWithNesting
                .Builder<Element, DefaultEdge, Module>()
                .setGraphicsProviderAndLabels(graphicsProvider, YedGmlWriter.PRINT_LABELS)
                .setGroups(collapsableGroups, Element::getName)
                .setVertexIdProvider(element -> String.valueOf(element.getFlowGraphNode().getId().hashCode()))
                .setEdgeLabelProvider(edge -> "")
                .setVertexLabelProvider(Element::getName)
                .build();
        final StringWriter writer = new StringWriter();
        gmlWriter.export(writer, graph);
        return writer.toString();
    }
}
