/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.rsql;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.LogicalNode;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.controller.module.filter.DependencyGraphFilterInputParameter;
import innowake.mining.server.controller.module.filter.DependencyGraphFilterParameters;
import innowake.mining.server.controller.module.filter.IDependencyGraphFilterParameter;
import innowake.mining.server.controller.module.filter.DependencyGraphFilterInputParameter.GraphProperty;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.dependency.graph.NodeType;

/**
 * This visitor handles the RSQL Model by processing all of its {@link Node}s and creating a {@link IDependencyGraphFilterParameter} based on the model.
 */
public class RsqlToDependencyGraphFilterVisitor implements RSQLVisitor<IDependencyGraphFilterParameter, Void> {

	@Override
	public IDependencyGraphFilterParameter visit(@Nullable final AndNode node, @Nullable final Void param) {
		return handleLogicalNode(assertNotNull(node));
	}

	@Override
	public IDependencyGraphFilterParameter visit(@Nullable final OrNode node, @Nullable final Void param) {
		return handleLogicalNode(assertNotNull(node));
	}

	@Override
	public IDependencyGraphFilterParameter visit(@Nullable final ComparisonNode node, @Nullable final Void param) {
		return handleFilter(assertNotNull(node));
	}

	private DependencyGraphFilterInputParameter handleFilter(final ComparisonNode node) {
		final GraphProperty graphProperty = GraphProperty.fromName(node.getSelector());
		final ComparisonOperator operator = node.getOperator();
		final List<String> arguments = node.getArguments();
		final List<Object> value;

		if (RsqlSearchOperation.fromOperator(operator) == RsqlSearchOperation.IN) {
			switch (graphProperty) {
				case MODULES:
					value = arguments.stream().map(NodeType::of).collect(Collectors.toList());
					break;
				case RELATIONSHIPS:
					value = arguments.stream().map(RelationshipType::from).collect(Collectors.toList());
					break;
				default:
					value = arguments.stream().collect(Collectors.toList());
					break;
			}
		} else {
			throw new IllegalArgumentException("The compare operator " + operator + " is not allowed.");
		}
		return new DependencyGraphFilterInputParameter(graphProperty, operator, value);
	}

	private IDependencyGraphFilterParameter handleLogicalNode(final LogicalNode logicalNode) {
		final List<IDependencyGraphFilterParameter> filters = logicalNode.getChildren()
				                                                         .stream()
				                                                         .map(this::createFilter)
				                                                         .filter(Objects::nonNull)
				                                                         .collect(Collectors.toList());
 
		final DependencyGraphFilterParameters result = new DependencyGraphFilterParameters();
		for (final IDependencyGraphFilterParameter filter : filters) {
			if (filter instanceof DependencyGraphFilterInputParameter) {
				final DependencyGraphFilterInputParameter inputFilter = (DependencyGraphFilterInputParameter) filter;
				switch (inputFilter.getProperty()) {
					case MODULES:
						result.addModuleNodeTypeFilter(inputFilter.getValue().stream()
								                                             .map(value -> (NodeType) value)
								                                             .collect(Collectors.toList()));
						break;
					case RELATIONSHIPS:
						result.addRelationshipFilter(inputFilter.getValue().stream()
								                                           .map(value -> RelationshipType.from(String.valueOf(value)))
								                                           .collect(Collectors.toList()));
						break;
					default:
						throw new IllegalArgumentException("Invalid filter property: " + inputFilter.getProperty());
				}
			}
		}
		return result;
	}

	@Nullable
	private IDependencyGraphFilterParameter createFilter(final Node node) {
		if (node instanceof LogicalNode) {
			return handleLogicalNode((LogicalNode) node);
		} else if (node instanceof ComparisonNode) {
			return handleFilter((ComparisonNode) node);
		} else {
			return null;
		}
	}
}
