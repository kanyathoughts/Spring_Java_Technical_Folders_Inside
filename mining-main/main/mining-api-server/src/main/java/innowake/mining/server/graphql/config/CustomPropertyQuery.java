/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.config;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Defines GraphQL queries supporting custom properties.
 */
public enum CustomPropertyQuery {
	
	MODULE(MiningEnitityNames.MODULE, MiningGraphQLQueries.MODULES,
			new HashSet<>(Arrays.asList(Usages.MINING_UI_MODULES_TABLE))),
	ANNOTATION(MiningEnitityNames.ANNOTATION, MiningGraphQLQueries.ANNOTATIONS,
			new HashSet<>(Arrays.asList(Usages.MINING_UI_ANNOTATIONS_TABLE))),
	DATA_DICTIONARY(MiningEnitityNames.DATA_DICTIONARY, MiningGraphQLQueries.DATA_DICTIONARY,
			new HashSet<>(Arrays.asList(Usages.MINING_UI_DATADICTIONARY_TABLE)));
	
	private final String entityName;
	private final String queryName;
	private final Set<String> defaultUsages;
	
	private CustomPropertyQuery(String entityName, String queryName, Set<String> defaultUsages) {
		this.entityName = entityName;
		this.queryName = queryName;
		this.defaultUsages = Collections.unmodifiableSet(defaultUsages);
	}
	
	/**
	 * @return Name of the data point entity.
	 */
	public String getEntityName() {
		return entityName;
	}
	
	/**
	 * @return Name of the QraphQL query.
	 */
	public String getQueryName() {
		return queryName;
	}
	
	/**
	 * @return Default usages for the custom properties data point.
	 */
	public Set<String> getDefaultUsages() {
		return defaultUsages;
	}
	
}
