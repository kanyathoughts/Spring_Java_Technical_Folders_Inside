/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import graphql.execution.RawVariables;
import graphql.execution.ValuesResolver;
import graphql.language.TypeName;
import graphql.language.VariableDefinition;
import org.springframework.graphql.execution.GraphQlSource;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
public class FilterObjectCoercionService {

	private static final String FILTER_OBJECT = "filterObject";
	private final GraphQlSource graphQlSource;
	private final ValuesResolver valuesResolver = new ValuesResolver();

	public FilterObjectCoercionService(final GraphQlSource graphQlSource) {
		this.graphQlSource = graphQlSource;
	}

	@SuppressWarnings("unchecked")
	public Map<String, Object> coerceFilterObject(final String queryName, final Map<String, Object> filterObject) {
		final TypeName typeName = new TypeName("FilterObject_" + queryName);
		final RawVariables rawVariables = new RawVariables(Map.of(FILTER_OBJECT, filterObject));
		final VariableDefinition variableDefinition = new VariableDefinition(FILTER_OBJECT, typeName);
		final Map<String, Object> coercedObject = valuesResolver.coerceVariableValues(graphQlSource.schema(), List.of(variableDefinition), rawVariables).toMap();
		return (Map<String, Object>) coercedObject.get(FILTER_OBJECT);
	}
}
