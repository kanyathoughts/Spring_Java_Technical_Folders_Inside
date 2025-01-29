/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.javatuples.Triplet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.core.OParseResult;
import innowake.mining.data.core.api.Model;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.query.DataFlowGraphQueryService;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.codeviewer.CodeViewerLink;
import innowake.mining.shared.model.codeviewer.CodeViewerLinkModel;
import innowake.mining.shared.model.codeviewer.CodeViewerRange;
import innowake.mining.shared.model.codeviewer.LinkTargetType;
import innowake.mining.shared.model.codeviewer.LinkType;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.ndt.core.parsing.IDocument;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.RetracedLocation;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Service for calculating a {@link CodeViewerLinkModel} using Data Lineage for a certain field.
 */
@Service
public class CodeViewerDataLineageService {

	private static final Logger LOG = LoggerFactory.getLogger(CodeViewerDataLineageService.class);
	
	@Autowired
	private DataFlowGraphQueryService dataFlowGraphQueryService;

	@Autowired
	private MiningDataCoreService core;
	
	@Autowired
	private CodeViewerLinkService codeViewerLinkService;

	/**
	 * Returns a {@link CodeViewerLinkModel} containing <i>only</i> the links to the given field's direct neighbors in the DataFlowGraph. In other words,
	 * this will provide links to all immediate the read/write accesses of a field.
	 * @param projectId the projectId
	 * @param moduleId the id of the module containing the field
	 * @param offset the offset of the field within the module
	 * @param assembled whether the offset should be interpreted as an assembled offset
	 * @return the link model for the field's read/write accesses
	 */
	public CodeViewerLinkModel getDataFlowLinksForField(final EntityId projectId, final EntityId moduleId, final Integer offset, final boolean assembled) {
		@Nullable
		final AstModel astModel;
		final EntityId actualModuleId;
		final Integer actualOffset;

		if (assembled) {
			final Optional<Triplet<EntityId, Integer, AstModel>> actualModuleIdAndOffset = getActualModuleIdAndOffset(moduleId, offset);
			if (actualModuleIdAndOffset.isEmpty()) {
				return new CodeViewerLinkModel(Collections.emptyList());
			}

			final Triplet<EntityId, Integer, AstModel> actualModuleInfo = actualModuleIdAndOffset.get();
			actualModuleId = actualModuleInfo.getValue0();
			actualOffset = actualModuleInfo.getValue1();
			astModel = actualModuleInfo.getValue2();
		} else {
			astModel = null;
			actualModuleId = moduleId;
			actualOffset = offset;
		}
		
		final var dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.addStartField(actualModuleId, actualOffset)
				.setMaxRecursionDepth(2) /* 0 produces an empty graph, 1 would contain only the start field, 2 returns start field plus direct neighbors */
				.setDetailLevel(DetailLevel.STATEMENT)
				.build());
		final IDocument document = getDocument(projectId, moduleId, astModel);

		return createCodeViewerLinkModel(dataFlowGraph, assembled, actualModuleId, astModel, moduleId, offset, document);
	}
	
	/**
	 * Returns {@link DataFlowGraph} containing <i>only</i> the nodes associated with the given field in the DataFlowGraph. In other words,
	 * this will provide nodes to all immediate the read/write accesses of a field and its related fields and flow the data between modules.
	 * @param projectId the projectId
	 * @param moduleId the id of the module containing the field
	 * @param offset the offset of the field within the module
	 * @param assembled whether the offset should be interpreted as an assembled offset
	 * @param includingModuleId the id of the Cobol program that includes the CopyBook module.
	 * @return the DataFlowGraph for the field's read/write accesses
	 */
	public DataFlowGraph getDataFlowGraphForField(final EntityId projectId, final EntityId moduleId, final Integer offset, final boolean assembled,
			@Nullable final EntityId includingModuleId) {
		return dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.addStartField(moduleId, offset, includingModuleId)
				.setDetailLevel(DetailLevel.STATEMENT)
				.setAssembled(assembled)
				.build());
	}
	
	private Optional<Triplet<EntityId, Integer, AstModel>> getActualModuleIdAndOffset(final EntityId moduleId, final Integer offset) {
		@Nullable
		final AstModel astModel;
		final EntityId actualModuleId;
		final Integer actualOffset;
		final AdvancedLocationProvider<ModulePojo> locationProvider;
		final RetracedLocation<ModulePojo> retracedLocation;
		/* it is unnecessary to parse the module again, but it is the simplest way to get access to the Assembling
		 * for all languages */
		final Optional<OParseResult> parseResult = core.getParseResult(moduleId);
		if (parseResult.isEmpty()) {
			return Optional.empty();
		}
		final Optional<Model> model = parseResult.get().getModel();
		if (model.isEmpty()) {
			return Optional.empty();
		}
		astModel = model.get().getParseModel();
		final Optional<AstNode> root = astModel.getRoot();
		if (root.isEmpty()) {
			return Optional.empty();
		}

		final List<AstNode> nodes = getAllChildNodesSorted(root);
		final Optional<AstNode> matchingNode = findMatchingNode(nodes, offset);

		if (matchingNode.isEmpty()) {
			return Optional.empty();
		}
		locationProvider = astModel.getAdvancedLocationProvider(ModulePojo.class);
		if (locationProvider == null) {
			return Optional.empty();
		}
		
		try {
			retracedLocation = locationProvider.getRetracedLocation(matchingNode.get());
		} catch (final Exception e) {
			LOG.warn(() -> String.format("storeAst: Unable to get location for node %s. Original error was: %s", matchingNode.get(), e));
			return Optional.empty();
		}
		if (retracedLocation == null) {
			return Optional.empty();
		}
		
		actualModuleId = retracedLocation.getPhyiscalModule().identity();
		actualOffset = Integer.valueOf(retracedLocation.getOffset());
		return Optional.of(Triplet.with(actualModuleId, actualOffset, astModel));
	}

	private Optional<AstNode> findMatchingNode(final List<AstNode> nodes, final Integer offset) {
		Optional<AstNode> matchingNode = Optional.empty();
		
		for (var i = 0; i < nodes.size(); i++) {
			final AstNode currentNode = nodes.get(i);
			final int startOffset;
			try {
				startOffset = currentNode.getStartOffset();
			} catch (final Exception e) {
				/* some "artificial" nodes (Cobol parser only, I think) throw an exception when calling getStartOffset() -.-
				 * we can ignore those nodes and the Exception */
				continue;
			}
			if (startOffset > offset.intValue()) {
				if (i == 0) {
					matchingNode = Optional.of(nodes.get(0));
				} else {
					/* If we are over the start offset we need to take the node to the left */
					matchingNode = Optional.of(nodes.get(i - 1));
				}
				break;
			}
		}
		return matchingNode;
	}

	private List<AstNode> getAllChildNodesSorted(final Optional<AstNode> root) {
		if (root.isEmpty()) {
			return Collections.emptyList();
		}

		return root.get()
				.getChildrenDeep(AstNode.class)
				.stream()
				.sorted((node1, node2) -> {
					try {
						final int start1Offset = node1.getStartOffset();
						final int start2Offset = node2.getStartOffset();

						if (start1Offset < 0 || start2Offset < 0) {
							return -1;
						}

						return start1Offset - start2Offset;
					} catch (final Exception e) {
						return -1;
					}
				})
				.collect(Collectors.toList());
	}

	private CodeViewerLinkModel createCodeViewerLinkModel(final DataFlowGraph dataFlowGraph, final boolean assembled, final EntityId actualModuleId, @Nullable final AstModel astModel, final EntityId moduleId, final Integer offset, final IDocument document) {
		/* we iterate over all fields in the returned graph and collect all of the read/write accesses - this way we will get the read/write accesses
		 * to the start field and all of its related fields */
		final List<CodeViewerLink> links = new ArrayList<>();

		dataFlowGraph.getNodes().stream()
		.filter(node -> DataFlowGraphNode.Type.FIELD.equals(node.getType()))
		.flatMap(node -> node.getOutgoings().stream())
		.flatMap(nodeId -> dataFlowGraph.getNodes().stream().filter(node -> nodeId.equals(node.getId())))
		.map(node -> {
			final ModuleLocation correctLocation;
			/* If we are trying to show the codeViewer in the assembled view we need to convert the "to"'s to the assembled values */
			if (assembled) {
				correctLocation = node.getLocation();
			} else {
				correctLocation = assertNotNull(node.getSourceLocation()).getModuleLocation();
			}

			return new CodeViewerLink(
					LinkType.DATA_FLOW_READ_ACCESS,
					LinkTargetType.LOCAL,
					"reads",
					node.getName(),
					moduleId,
					new ModuleLocation(offset.intValue(), 1),
					convertEditorRange(document, new ModuleLocation(offset.intValue(), 1)),
					actualModuleId,
					correctLocation,
					convertEditorRange(document, correctLocation)
					);
		})
		.forEach(links::add);

		dataFlowGraph.getNodes().stream()
		.filter(node -> DataFlowGraphNode.Type.FIELD.equals(node.getType()))
		.flatMap(node -> node.getIncomings().stream())
		.flatMap(nodeId -> dataFlowGraph.getNodes().stream().filter(node -> nodeId.equals(node.getId())))
		.map(node -> {
			final ModuleLocation correctLocation;
			/* If we are trying to show the codeViewer in the assembled view we need to convert the "to"'s to the assembled values */
			if (assembled) {
				correctLocation = codeViewerLinkService.getAssembledLocation(actualModuleId, node.getLocation(), Optional.of(astModel))
						.orElseGet(node::getLocation);
			} else {
				correctLocation = assertNotNull(node.getSourceLocation()).getModuleLocation();
			}

			return new CodeViewerLink(
					LinkType.DATA_FLOW_WRITE_ACCESS,
					LinkTargetType.LOCAL,
					"writes",
					node.getName(),
					moduleId,
					new ModuleLocation(offset.intValue(), 1),
					convertEditorRange(document, new ModuleLocation(offset.intValue(), 1)),
					actualModuleId,
					correctLocation,
					convertEditorRange(document, correctLocation)
					);
		}).forEach(links::add);
		
		return new CodeViewerLinkModel(links);
	}

	private IDocument getDocument(final EntityId projectId, final EntityId moduleId, @Nullable final AstModel model) {
		if (model != null) {
			return new Document(model.getSource());
		}
		
		final String content = core.moduleService.findAnyModule(q -> q.ofProject(projectId)
																	.byId(moduleId)
																	.includeContent(true))
				.orElseThrow(() -> new MiningEntityNotFoundException("Module not found for identifier: " + moduleId + " and project: " + projectId))
				.getContent().orElseThrow();
		return new Document(content);
	}

	@Nullable
	private CodeViewerRange convertEditorRange(final IDocument document, @Nullable final ModuleLocation moduleLocation) {
		if (moduleLocation == null) {
			return null;
		}
		final int startOffset = moduleLocation.getOffset();
		final int endOffset = moduleLocation.getOffset() + moduleLocation.getLength();
		/* converting to 1-based indexes here */
		return new CodeViewerRange(
				document.getLineNumber(startOffset) + 1,
				document.getColumnNumber(startOffset) + 1,
				document.getLineNumber(endOffset) + 1,
				document.getColumnNumber(endOffset) + 1);
	}
}
