/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.entities.ast.StoreAstRelationship;

public class AstTestUtil {
	
	public static String edgesToString(final List<StoreAstRelationship> edges, final String name, final Function<StoreAstRelationship, AstNodePojoPrototype> value) {
		final StringBuilder sb = new StringBuilder("{ ");
		final var iterator = edges.iterator();
		while (iterator.hasNext()) {
			final var edge = iterator.next();
			final var node = value.apply(edge);
			sb.append(edge.type.getNonNull().name()).append(" [").append(name).append("=")
				.append(node.type.orElse(null) + "[" + node.label.orElse(null) + "]");
			edge.label.ifDefined(l -> sb.append(", label=").append(l));
			sb.append("]");
			if (iterator.hasNext()) {
				sb.append(", ");
			}
		}
		return sb.append(" }").toString();
	}
	
	public static StringBuilder nodeToString(final StringBuilder sb, final StoreAstPrototype node) {
		sb.append("AstNode [type=").append(node.type.orElse(null));
		sb.append(", moduleId=").append(node.module.optional().map(EntityId::getNid).orElse(null));
		sb.append(", location=").append(node.location.orElse(null));
		sb.append(", inclusionLevel=").append(node.getInclusionLevel());
		sb.append(", inclusionOrdinal=").append(node.getInclusionOrdinal());
		
		node.includedModule.optional().map(EntityId::getNid).ifPresent(v -> sb.append(", inclusionCalleeModuleId=").append(v));
		
		node.getModuleRelationships().stream().filter(r -> AstModuleRelationshipType.ROOT.equals(r.type.orElse(null)))
			.forEach(r -> sb.append(", hasAstModuleId=").append(r.module.optional().map(EntityId::getNid).orElse(null)));
		
		if (! node.getRelationships().isEmpty()) {
			sb.append(", relations=").append(AstTestUtil.edgesToString(node.getRelationships(), "to", r -> r.dstNode.getNonNull()));
		}
		
		node.properties.optional().filter(p -> ! p.isEmpty()).ifPresent(p -> {
			sb.append(", properties=");
			final String mapAsString = p.keySet().stream().sorted(String.CASE_INSENSITIVE_ORDER).map(key -> key + "=" + p.get(key))
					.collect(Collectors.joining(", ", "[", "]"));
			sb.append(mapAsString);
		});
		
		return sb.append("]");
	}
	
}
