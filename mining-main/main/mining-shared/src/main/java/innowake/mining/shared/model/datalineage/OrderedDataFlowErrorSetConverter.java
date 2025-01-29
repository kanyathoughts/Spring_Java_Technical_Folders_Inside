/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.util.StdConverter;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Converter converting a Set of {@linkplain DataFlowErrorPojo} to an {@linkplain LinkedHashSet} (which is ordered). This is used to retain the order
 * when JSON-serializing the errors property of {@linkplain DataFlowGraphNode}.
 */
public class OrderedDataFlowErrorSetConverter extends StdConverter<Set<DataFlowErrorPojo>, Set<DataFlowErrorPojo>> {

	@Override
	public Set<DataFlowErrorPojo> convert(@Nullable final Set<DataFlowErrorPojo> value) {
		return value == null ? new TreeSet<>()
				: value.stream().sorted(Comparator.nullsLast(Comparator.naturalOrder()))
				.collect(Collectors.toCollection(LinkedHashSet::new));
	}
}
