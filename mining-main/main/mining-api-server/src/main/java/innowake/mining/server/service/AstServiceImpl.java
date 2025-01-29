/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.AnnotationPgDao;
import innowake.mining.data.access.postgres.AstPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.controlflow.ControlFlowEdge;
import innowake.mining.shared.model.controlflow.ControlFlowEntity;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.controlflow.ControlFlowNode;
import innowake.ndt.core.parsing.ast.model.cfg.CfgCollapsibleNode;

/**
 * Central point for accessing and modifying Client entities.
 */
@Service
public class AstServiceImpl implements AstService {

	private final AstPgDao astDao;
	private final AnnotationPgDao annotaionDao;
	private final ModulePgDao moduleDao;

	@Autowired
	public AstServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		astDao = new AstPgDao(jdbcTemplate);
		annotaionDao = new AnnotationPgDao(jdbcTemplate);
		moduleDao = new ModulePgDao(jdbcTemplate);
	}

	@Override
	public UUID create(final AstNodePojoPrototype prototype) {
		return astDao.put(prototype, true);
	}

	@Override
	public UUID update(final AstNodePojoPrototype prototype) {
		return astDao.put(prototype, false);
	}
	
	@Override
	public AstNodePojo get(final UUID id) {
		return astDao.findAny(q -> q.byId(id)).orElseThrow(() -> new MiningEntityNotFoundException(AstNodePojo.class, id.toString()));
	}
	
	@Override
	public List<AstNodePojo> find(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return astDao.find(builder);
	}

	@Override
	public List<UUID> findIds(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return astDao.findIds(builder);
	}

	@Override
	public long count(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return astDao.count(builder);
	}

	@Override
	public int delete(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return astDao.delete(builder);
	}

	@Override
	public List<AstRelationshipPojo> findRelationships(final BuildingConsumer<AstRelationshipInquiryBuilder> builder) {
		return astDao.findRelationships(builder);
	}

	@Override
	public Optional<AstNodePojo> findOne(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return astDao.findOne(builder);
	}

	@Override
	public Optional<AstNodePojo> findAny(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return astDao.findAny(builder);
	}

	@Override
	public List<AstModuleRelationshipPojo> findModuleRelationships(final BuildingConsumer<AstModuleRelationshipInquiryBuilder> builder) {
		return astDao.findModuleRelationships(builder);
	}

	@Override
	public UUID createRelationship(final UUID src, final UUID dst, final AstRelationshipType type) {
		return createRelationship(new AstRelationshipPojoPrototype().setSrc(src).setDst(dst).setType(type));
	}

	@Override
	public UUID createRelationship(final AstRelationshipPojoPrototype relationship) {
		return astDao.createRelationship(relationship);
	}

	@Override
	public void createModuleRelationship(final AstModuleRelationshipPojoPrototype relation) {
		astDao.createModuleRelationship(relation);
	}

	@Override
	public int deleteRelationshipsByModule(final EntityId moduleId, final Collection<AstRelationshipType> types) {
		return astDao.deleteRelationshipsByModule(moduleId, types);
	}

	@Override
	public int deleteModuleRelationships(final EntityId moduleId, final Collection<AstModuleRelationshipType> types) {
		return astDao.deleteModuleRelationships(moduleId, types);
	}

	@Override
	public ControlFlowGraph getControlFlow(final EntityId module, @Nullable final Integer maxLabelLength) {
		final var moduleIds = new HashSet<EntityId>();
		final var graph = new ControlFlowGraph();
		
		graph.annotations.addAll(annotaionDao.find(q -> q.ofModule(module)));
		final Map<UUID, ControlFlowNode> cfNodes = new HashMap<>();
		
		astDao.find(q -> {
			q.ofModule(module);
			q.withRelationshipTypes(Arrays.asList(AstRelationshipType.FLOW), AstModuleRelationshipType.CONTROL_FLOW_TERMINALS);
			if (maxLabelLength != null) {
				q.limitLabelLength(maxLabelLength);
			}
			q.sortRetracedLocation();
		}).forEach(astNode -> {
			final var cfNode = new ControlFlowNode(astNode.getId(), astNode.getModuleUid(), ControlFlowEntity.AST_NODE);
			cfNode.type = astNode.getType();
			cfNode.label = astNode.getLabel();
			cfNode.offset = astNode.getLocation().getRetracedOffset().orElse(null);
			cfNode.length = astNode.getLocation().getRetracedLength().orElse(null);
			cfNode.parent = astNode.getParentId().orElse(null);
			cfNode.superTypes = astNode.getSuperTypes();
			cfNode.properties.putAll(astNode.getProperties());
			
			moduleIds.add(astNode.getModule());
			astNode.getIncludedModule().ifPresent(moduleIds::add);
			
			AstService.Properties.FILE_IDS_IN.optionalFrom(astNode.getProperties()).ifPresent(nids ->
				nids.stream().map(Number::longValue).map(EntityId::of).forEach(moduleIds::add));
			AstService.Properties.FILE_IDS_OUT.optionalFrom(astNode.getProperties()).ifPresent(nids ->
				nids.stream().map(Number::longValue).map(EntityId::of).forEach(moduleIds::add));
			
			Stream.concat(astNode.getIncomingRelations().stream(), astNode.getOutgoingRelations().stream())
				.filter(r -> r.getType().equals(AstRelationshipType.FLOW))
				.forEach(astEdge -> graph.edges.add(new ControlFlowEdge(astEdge.getSrc(), astEdge.getDst(), astEdge.getLabel().orElse(null))));

			cfNodes.put(astNode.getId(), cfNode);
			graph.nodes.add(cfNode);
		});

		final Map<ControlFlowNode, UUID> parentUpdates = new HashMap<>();
		final List<ControlFlowNode> cfAnnotationNodes = new ArrayList<>(graph.annotations.size());
		for (final var annotation : graph.annotations) {
			annotation.getLocation().ifPresent(al -> {
				final var cfAnnotationNode = new ControlFlowNode(annotation.getUid(), annotation.getModuleUid(), ControlFlowEntity.ANNOTATION);
				cfAnnotationNode.superTypes = new HashSet<>(Arrays.asList(CfgCollapsibleNode.class.getSimpleName()));
				cfAnnotationNode.type = annotation.getType().getDisplayName();
				cfAnnotationNode.label = annotation.getName();
				cfAnnotationNode.offset = annotation.getLocation().map(ModuleLocation::getOffset).orElse(null);
				cfAnnotationNode.length = annotation.getLocation().map(ModuleLocation::getLength).orElse(null);

				/* We add a cf node for the annotation into the graph. For the most outer ast cf node, whose offset equals with the annotation offset,
				 * we do the following:
				 * -> the id of the annotation cf node is set as the new parent of the matching ast cf node
				 * -> If the parent of the matching ast cf node is present in the graph, its id is set as the parent of the annotation cf node.
				 *    -> If the parent is not present, then set the parent of the matching ast cf node as the parent of the annotation cf node */
				for (final var cfNode : graph.nodes) {
					var parentId = cfNode.parent;
					final Integer annOffset = cfAnnotationNode.offset;
					if (parentId != null && annOffset != null && annOffset.equals(cfNode.offset)) {
						var parent = cfNodes.get(parentId);
						if (parent != null && annOffset.equals(parent.offset)) {
							/* in OrientDB we got the most outer nodes first, in Postgres we have to find them */
							parent = getMostOuterParent(cfNodes, parent);
							parentId = parent.parent;
							parentUpdates.put(parent, cfAnnotationNode.id);
						}

						cfAnnotationNode.parent = parentId;
						if (cfNode.parent == parentId) {
							/* cfNode was already the most outer AstNode */
							parentUpdates.put(cfNode, cfAnnotationNode.id);
						}
						break;
					}
				}

				cfAnnotationNodes.add(cfAnnotationNode);
			});
		}
		
		parentUpdates.entrySet().forEach(entry -> entry.getKey().parent = entry.getValue());
		graph.nodes.addAll(cfAnnotationNodes);

		astDao.findModuleRelationships(q -> q.ofModule(module).withTypes(AstModuleRelationshipType.CONTROL_FLOW_TERMINALS))
			.forEach(rel -> {
				final var id = UUID.randomUUID();
				final var cfNode = new ControlFlowNode(id, rel.getModule(), ControlFlowEntity.TERMINAL);
				cfNode.type = rel.getType().name();
				graph.nodes.add(cfNode);
				if (RelationshipDirection.IN.equals(rel.getType().getDirection())) {
					graph.edges.add(new ControlFlowEdge(id, rel.getNode()));
				} else {
					graph.edges.add(new ControlFlowEdge(rel.getNode(), id));
				}
			});
		
		graph.relatedModules.addAll(moduleDao.findModulesLightweight(q -> q.byIds(moduleIds)));
		
		return graph;
	}

	private ControlFlowNode getMostOuterParent(final Map<UUID, ControlFlowNode> cfNodes, final ControlFlowNode node) {
		if (node.parent != null) {
			final ControlFlowNode parent = cfNodes.get(node.parent);
			if (parent != null && parent.offset != null && parent.offset.equals(node.offset)) {
				return getMostOuterParent(cfNodes, parent);
			}
		}

		return node;
	}
}
