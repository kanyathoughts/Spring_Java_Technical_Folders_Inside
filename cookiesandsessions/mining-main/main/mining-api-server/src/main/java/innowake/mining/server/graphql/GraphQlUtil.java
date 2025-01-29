/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.stream.Stream;

import graphql.language.Field;
import graphql.schema.DataFetcher;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.SelectedField;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.shared.entities.ModulePojo;

public class GraphQlUtil {
	
	private GraphQlUtil() { }
	
	/**
	 * Checks if there are {@link innowake.mining.data.datapoints.MiningDataPointBuilder.DynamicFieldBuilder} present on any of the selected data-points
	 * and applies them to the current query.
	 * @param <T> Type of the query builder.
	 * @param reg Registry of currently valid data-points.
	 * @param project ID of a project, specific data-points of which are applicable to the request.
	 * @param referenceType Root data point for which {@link innowake.mining.data.datapoints.MiningDataPointBuilder.DynamicFieldBuilder}s have been registered.
	 * @param path Path to the root data point relative to the {@link DataFetchingEnvironment}. This is empty in case the root type is also the result type of the query.
	 * @param env GraphQL environment of the query.
	 * @param queryBuilder Data access builder.
	 * @param plainFields List of field paths to add to the query if they have not been selected but shall still be available for filtering or sorting.
	 */
	public static <T> void applyDynamicSelection(final DataPointRegistry reg, @Nullable final Long project,
			final String referenceType, final String[] path, final DataFetchingEnvironment env, T queryBuilder, final Stream<String[]> plainFields) {
		final var selectedFields = new HashSet<String[]>();
		for (var field : env.getSelectionSet().getFields()) {
			final String[] fieldPath = relativizePath(path, field.getQualifiedName().split("/"));
			if (fieldPath.length > 0) {
				reg.getDataPointDefinitionAtPath(Optional.ofNullable(project), referenceType, fieldPath)
					.ifPresent(dp -> dp.getDynamicFieldBuilder(referenceType).ifPresent(d -> d.accept(GraphQlUtil.fieldReference(field), field.getArguments(), queryBuilder)));
				selectedFields.add(fieldPath);
			}
		}
		plainFields.forEach(field -> {
			final String[] fieldPath = relativizePath(path, field);
			if (! (fieldPath.length < 1 || selectedFields.contains(fieldPath))) {
				reg.getDataPointDefinitionAtPath(Optional.ofNullable(project), referenceType, fieldPath)
					.ifPresent(dp -> dp.getDynamicFieldBuilder(referenceType).ifPresent(d -> d.accept(field[field.length - 1], Collections.emptyMap(), queryBuilder)));
				selectedFields.add(fieldPath);
			}
		});
	}
	
	private static String[] relativizePath(final String[] prefix, final String[] path) {
		if (path.length > prefix.length && Arrays.equals(prefix, 0, prefix.length, path, 0, prefix.length)) {
			return Arrays.copyOfRange(path, prefix.length, path.length);
		}
		return new String[0];
	}
	
	public static <T> DataFetcher<T> fetchDynamicProperty() {
		return env -> env.<ModulePojo>getSource().dynamic(GraphQlUtil.fieldReference(env.getField()));
	}
	
	/**
	 * @param field Field definition.
	 * @return Field name or alias.
	 */
	public static String fieldReference(final Field field) {
		String ref = field.getAlias();
		if (ref == null) {
			ref = field.getName();
		}
		return ref;
	}
	
	/**
	 * @param field Field definition.
	 * @return Field name or alias.
	 */
	public static String fieldReference(final SelectedField field) {
		String ref = field.getAlias();
		if (ref == null) {
			ref = field.getName();
		}
		return ref;
	}
	
}
