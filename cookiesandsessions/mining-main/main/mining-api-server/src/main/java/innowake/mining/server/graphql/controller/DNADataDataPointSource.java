/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import innowake.mining.data.access.postgres.ModulePgDao.ModuleQueryBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.lucene.LuceneUtil;

/**
 * * Defines data points for {@link DNADataGraphQlController}.
 */
@Component
@Order(Ordered.LOWEST_PRECEDENCE)
public class DNADataDataPointSource implements MiningDataPointSource {

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineDataPointsFromSchemaMappingAnnotations(DNADataGraphQlController.class);
		
		builder.extend(MiningEnitityNames.MODULE, "id")
			.withSorting(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, ModuleQueryBuilder::sortId)
			.withFiltering(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, f -> f.<ModuleQueryBuilder, Object>eq((b, id) -> b.byId(FilterOperators.OPERATOR_EQ, id)))
			.add();
		builder.extend(MiningEnitityNames.MODULE, "path")
			.withSorting(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, ModuleQueryBuilder::sortPath)
			.add();
		
		builder.extend(MiningEnitityNames.MODULE, "name")
			.withSorting(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, ModuleQueryBuilder::sortName)
			.withFiltering(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, f -> {
					f.<ModuleQueryBuilder, String>eq((b, val) -> {
						//Complicated due to Flags. This is basically a withName() with some flags applied
						//toLowercase
						String fixedVal = val.toLowerCase();
						//beginsWith for LUCENE searches, '*' is used instead of '%' as the wildcard
						fixedVal = fixedVal.trim() + '*';
						//escapeLucene
						fixedVal = LuceneUtil.escapeSearchTerm(fixedVal, false);
						b.withName(fixedVal);
					});
				})
			.add();
	}
}
