/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints.registry;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.shared.datapoints.definition.AliasDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import org.apache.commons.lang.StringUtils;

import java.util.*;

class RegistryModel {
	public final Map<String, MiningDataTypeDefinition> typeDefinitions = new HashMap<>();
	public final Map<String, MiningEnumDefinition> enumDefinitions = new HashMap<>();
	public final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointDefinitions = new HashMap<>();

	public RegistryModel clone(final Optional<String> excludeProvider) {
		final RegistryModel model = new RegistryModel();
		this.typeDefinitions.entrySet().forEach(e -> {
			if (!(excludeProvider.isPresent() && e.getValue().isProvidedBy(excludeProvider.get()))) {
				model.typeDefinitions.put(e.getKey(), e.getValue());
			}
		});
		this.enumDefinitions.entrySet().forEach(e -> {
			if (!(excludeProvider.isPresent() && e.getValue().isProvidedBy(excludeProvider.get()))) {
				model.enumDefinitions.put(e.getKey(), e.getValue());
			}
		});
		this.dataPointDefinitions.entrySet().forEach(e -> e.getValue().entrySet().forEach(def -> {
			if (!(excludeProvider.isPresent() && def.getValue().isProvidedBy(excludeProvider.get()))) {
				model.dataPointDefinitions.computeIfAbsent(e.getKey(), k -> new HashMap<>()).put(def.getKey(), def.getValue());
			}
		}));
		return model;
	}

	public void resolveAliasType(final MiningDataPointDefinitionWithCustomFetch dataPoint, final RegistryModel globalModel) {
		/* resolves the scalarType or referenceTypeName for alias definitions by looking up the type of the actual, aliased data point */
		final AliasDefinition aliasDefinition = dataPoint.getAliasFor();
		if (aliasDefinition == null) {
			/* not an alias */
			return;
		}
		final List<String> subSelectionPath = StringUtils.isEmpty(aliasDefinition.getSubSelection()) ? Collections.emptyList()
				: Arrays.asList(aliasDefinition.getSubSelection().split("\\."));
		final MiningDataPointDefinitionWithCustomFetch aliasedDataPoint = lookupAliasedDataPoint(dataPoint.getParentTypeName(), aliasDefinition.getAliasFor(),
				subSelectionPath, globalModel);

		if (aliasedDataPoint == null) {
			throw new IllegalArgumentException("The alias definition " + dataPoint.getName() + " on type " + dataPoint.getParentTypeName()
					+ " provided by " + dataPoint.getProvidedBy()
					+ " is invalid: the target aliased data point " + aliasDefinition.getAliasFor() + "." + aliasDefinition.getSubSelection()
					+ " can not be found.");
		}

		/* update the existing alias definition with the correct type*/
		dataPoint.setScalarType(aliasedDataPoint.getScalarType());
		dataPoint.setReferenceTypeName(aliasedDataPoint.getReferenceTypeName());
		dataPoint.setArray(aliasedDataPoint.isArray());
		dataPoint.setNullable(aliasedDataPoint.isNullable());
	}

	@Nullable
	public MiningDataPointDefinitionWithCustomFetch lookupAliasedDataPoint(final String currentTypeName, final String currentDataPointName,
																		   final List<String> subSelectionPath, final RegistryModel globalModel) {

		final var projectSpecificType = dataPointDefinitions.get(currentTypeName);
		final var globalType = globalModel.dataPointDefinitions.get(currentTypeName);

		MiningDataPointDefinitionWithCustomFetch currentDataPoint = null;
		if (projectSpecificType != null) {
			currentDataPoint = projectSpecificType.get(currentDataPointName);
		}
		if (currentDataPoint == null && globalType != null) {
			currentDataPoint = globalType.get(currentDataPointName);
		}

		if (currentDataPoint == null || subSelectionPath.isEmpty()) {
			return currentDataPoint;
		}


		return lookupAliasedDataPoint(currentDataPoint.getReferenceTypeName(), subSelectionPath.get(0),
				subSelectionPath.subList(1, subSelectionPath.size()), globalModel);
	}
}
