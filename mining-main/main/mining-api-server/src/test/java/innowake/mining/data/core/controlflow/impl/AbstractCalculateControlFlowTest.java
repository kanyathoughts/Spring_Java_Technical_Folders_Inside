/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.junit.jupiter.api.BeforeAll;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.controlflow.api.AstToControlFlow;
import innowake.mining.data.core.storeast.impl.AbstractStoreAstTest;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.Technology;

/**
 * Base class for control flow tests.
 */
public abstract class AbstractCalculateControlFlowTest {
	
	protected final AbstractStoreAstTest storeAstTest;
	protected final Technology technology;
	
	protected AbstractCalculateControlFlowTest(final AbstractStoreAstTest storeAstTest, final Technology technology) {
		this.storeAstTest = storeAstTest;
		this.technology = technology;
	}
	
	@BeforeAll
	static void setup() {
		ToStringBuilder.setDefaultStyle(ToStringStyle.MULTI_LINE_STYLE);
	}

	/**
	 * Creates the control flow for a given module and compares it with an expected file.
	 * 
	 * @param folderName name of the folder where the module is located
	 * @param testName Name of the test used for the expected file
	 * @param moduleNames name of the modules
	 * 
	 */
	protected void doTest(final String folderName, final String testName, final String... moduleNames) {
		final var ast = storeAstTest.createAst(folderName, moduleNames);
		final AstNodePojo rootNode = ast.get(null);
		final Map<AstNodePojo, Long> astIndex = new AstNodeIndexer().traverse(rootNode);
		
		final var cf = AstToControlFlow.getNewInstance(technology, rootNode);
		cf.calculateControlFlow();
		
		final ControlFlowAstToString toStringBuilder = new ControlFlowAstToString(
				new NodeToStringFunction(astIndex, ast::get, cf.getFlow(), cf.getTerminals()));
		final Optional<AstNodePojo> procedureDivision = extractStartNodeForPrinting(rootNode);
		if (procedureDivision.isPresent()) {
			final String output = toStringBuilder.traverse(procedureDivision.get()).toString();
			storeAstTest.assertOutput(folderName, testName, output);
		} else {
			fail("Start node was not found");
		}
	}

	protected abstract Optional<AstNodePojo> extractStartNodeForPrinting(AstNodePojo rootNode);
	
	protected abstract StringBuilder languageSpecificNodeHandling(Map<AstNodePojo, Long> astIndex, AstNodePojo nodeNonNull);
	
	protected String vertexToString(final Map<AstNodePojo, Long> astIndex, AstNodePojo node) {
		return new StringBuilder("#").append(astIndex.get(node)).toString();
	}
	
	private class NodeToStringFunction implements Function<AstNodePojo, String> {

		private final Map<AstNodePojo, Long> astIndex;
		private Map<UUID, List<String>> relationsTo;
		private Map<UUID, List<String>> relationsFrom;

		private NodeToStringFunction(final Map<AstNodePojo, Long> astIndex, final Function<UUID, AstNodePojo> nodeMapper,
				final Collection<ControlFlowPrototype> relations, final Collection<AstModuleRelationshipPojoPrototype> moduleRelations) {
			this.astIndex = astIndex;
			relationsTo = new HashMap<>();
			relationsFrom = new HashMap<>();
			for (final var relation : relations) {
				relationsTo.computeIfAbsent(relation.dst.get(), k -> new ArrayList<>())
					.add(vertexToString(astIndex, nodeMapper.apply(relation.src.get())) + getEdgeLabel(relation));
				relationsFrom.computeIfAbsent(relation.src.get(), k -> new ArrayList<>())
					.add(vertexToString(astIndex, nodeMapper.apply(relation.dst.get())) + getEdgeLabel(relation));
			}
			for (final var relation : moduleRelations) {
				final String type;
				switch (relation.type.getNonNull()) {
					case ENTRY: type = "TestEntryPoint"; break;
					case HALT: type = "TestHaltPoint"; break;
					case RETURN: type = "TestReturnPoint"; break;
					default: type = "UNKNOWN";
				}
				(RelationshipDirection.IN.equals(relation.type.getNonNull().getDirection()) ? relationsTo : relationsFrom) 
					.computeIfAbsent(relation.node.get(), k -> new ArrayList<>()).add(type);
			}
		}

		@Override
		public String apply(@Nullable final AstNodePojo node) {
			final AstNodePojo nodeNonNull = assertNotNull(node);
			final StringBuilder sb = languageSpecificNodeHandling(astIndex, nodeNonNull);
			if (sb.length() == 0) {
				return "";
			}

			final var inbound = relationsTo.get(nodeNonNull.getId());
			if (inbound != null &&  ! inbound.isEmpty()) {
				final String fromEdges = edgesToString(inbound);
				if ( ! "{}".equals(fromEdges)) {
					sb.append(", FROM=").append(fromEdges);
				}
			}
			
			final var outbound = relationsFrom.get(nodeNonNull.getId());
			if (outbound != null && ! outbound.isEmpty()) {
				final String toEdges = edgesToString(outbound);
				if ( ! "{}".equals(toEdges)) {
					sb.append(", TO=").append(toEdges);
				}
			}
			
			return sb.toString();
		}

		private String edgesToString(final Collection<String> edges) {
			final StringBuilder sb = new StringBuilder("{");
			final Iterator<String> iterator = edges.stream().sorted().iterator();
			while (iterator.hasNext()) {
				final String edge = iterator.next();
				sb.append(edge);
				if (iterator.hasNext()) {
					sb.append(", ");
				}
			}
			return sb.append("}").toString();
		}

		private String getEdgeLabel(final AstRelationshipPojoPrototype relation) {
			Optional<String> label = Optional.empty();
			if (AstRelationshipType.FLOW.equals(relation.type.get())) {
				label = relation.label.optional();
			}
			return label.isPresent() ? "(" + label.get() + ")" : "";
		}
	}

}