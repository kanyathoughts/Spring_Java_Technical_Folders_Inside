/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.module.filter;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import cz.jirutka.rsql.parser.RSQLParser;
import cz.jirutka.rsql.parser.ast.Node;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.server.controller.rsql.RsqlToDependencyGraphFilterVisitor;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.dependency.graph.NodeType;

/**
 * A Parser class to help parse the Filter Query used to filter Dependency Graph.
 */
public class DependencyGraphFilterParser {
	
	private DependencyGraphFilterParser() {
		/* Empty constructor as it contains only static methods. */
	}
	
	/**
	 * Method to parse the filter query that is to be used while filtering the Dependency Graph.
	 *
	 * @param rawFilter - query that is to be used for filtering as a {@link String}
	 * @return {@link Tuple2} of {@link NodeType} and {@link RelationshipType} to filter the graph
	 */
	public static Tuple2<List<NodeType>, List<RelationshipType>> parseFilterQuery(final Optional<String> rawFilter) {
		final List<NodeType> nodeTypesToFilter = new ArrayList<>();
		final List<RelationshipType> relationshipsToFilter = new ArrayList<>();
		if (rawFilter.isPresent()) {
			final Node node = new RSQLParser().parse(rawFilter.get());
			final IDependencyGraphFilterParameter filter = node.accept(new RsqlToDependencyGraphFilterVisitor());
			final DependencyGraphFilterParameters filterGroup = getFilterParameters(filter);
			if (filterGroup != null) {
				nodeTypesToFilter.addAll(filterGroup.getModuleNodeTypeFilter());
				relationshipsToFilter.addAll(filterGroup.getRelationshipFilter());
			}
		}
		return Tuple2.of(nodeTypesToFilter, relationshipsToFilter);
	}

	@Nullable
	private static DependencyGraphFilterParameters getFilterParameters(final IDependencyGraphFilterParameter filter) {
		if (filter instanceof DependencyGraphFilterInputParameter) {
			final DependencyGraphFilterInputParameter inputFilter = (DependencyGraphFilterInputParameter) filter;
			final DependencyGraphFilterParameters filterResult = new DependencyGraphFilterParameters();
			switch (inputFilter.getProperty()) {
				case MODULES:
					filterResult.addModuleNodeTypeFilter(inputFilter.getValue().stream()
					                                                           .map(filterValue -> (NodeType) filterValue)
					                                                           .collect(Collectors.toList()));
					break;
				case RELATIONSHIPS:
					filterResult.addRelationshipFilter(inputFilter.getValue().stream()
					                                                         .map(filterValue -> RelationshipType.from(String.valueOf(filterValue)))
					                                                         .collect(Collectors.toList()));
					break;
				default:
					break;
			}
			return filterResult;
		} else if (filter instanceof DependencyGraphFilterParameters) {
			return (DependencyGraphFilterParameters) filter;
		} else {
			return null;
		}
	}
}
