/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints.registry;

import com.google.common.collect.Streams;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import innowake.mining.shared.datapoints.definition.MiningSchemaClass;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

public class RegistryView {
	public final RegistryModel globalModel;
	public final Map<Long, RegistryModel> projectModels;
	public final Map<String, MiningDataPointDefinition> queryDefinitions = new HashMap<>();

	public RegistryView() {
		this(new RegistryModel(), null);
	}

	private RegistryView(final RegistryModel globalModel) {
		this(globalModel, null);
	}

	private RegistryView(final RegistryModel globalModel, @Nullable final Map<Long, RegistryModel> projectModels) {
		this.globalModel = globalModel;
		this.projectModels = projectModels == null ? new ConcurrentHashMap<>() : projectModels;
	}

	public RegistryView clone(final Optional<String> excludeProvider, final Optional<Long> projectId) {
		final RegistryView newRegistry = new RegistryView(projectId.isPresent() ? globalModel : globalModel.clone(excludeProvider));
		projectModels.entrySet().forEach(e -> newRegistry.projectModels.put(e.getKey(),
				!projectId.isPresent() || e.getKey().equals(projectId.get()) ? e.getValue().clone(excludeProvider) : e.getValue()));
		if (projectId.isPresent() || !excludeProvider.isPresent()) {
			newRegistry.queryDefinitions.putAll(queryDefinitions);
		} else {
			this.queryDefinitions.entrySet().forEach(e -> {
				if (!(e.getValue().isProvidedBy(excludeProvider.get()))) {
					newRegistry.queryDefinitions.put(e.getKey(), e.getValue());
				}
			});
		}
		return newRegistry;
	}

	public RegistryModel getModel(final Optional<Long> projectId) {
		final RegistryModel model = projectId.isPresent() ? projectModels.get(projectId.get()) : globalModel;
		if (model == null) {
			throw new NoSuchElementException((projectId.isPresent() ? "Model for Project " + projectId.get() : "Global Model") + " not present.");
		}
		return model;
	}

	public RegistryModel getOrCreateModel(final Optional<Long> projectId) {
		if (projectId.isPresent()) {
			RegistryModel projectModel = projectModels.get(projectId.get());
			if (projectModel == null) {
				projectModel = new RegistryModel();
				projectModels.put(projectId.get(), projectModel);
			}
			return projectModel;
		} else {
			return globalModel;
		}
	}

	public Optional<RegistryModel> getProjectModel(final Optional<Long> projectId) {
		if (projectId.isPresent()) {
			return Optional.ofNullable(projectModels.get(projectId.get()));
		}
		return Optional.empty();
	}

	public void forEachModel(final BiConsumer<Optional<Long>, RegistryModel> action) {
		action.accept(Optional.empty(), globalModel);
		projectModels.entrySet().forEach(e -> action.accept(Optional.of(e.getKey()), e.getValue()));
	}

	/**
	 * Returns a map of known java classes to the mining schema class. This map is populated when defining a data type through
	 * {@link innowake.mining.data.datapoints.MiningDataPointBuilder} using {@code defineType(...).representedBy(...)}.
	 * @return map of fully-qualified class names to schema classes.
	 */
	public Map<String, MiningSchemaClass> getKnownClasses() {
		final Map<String, MiningSchemaClass> knownClasses = new HashMap<>();
		forEachModel((projectId, model) -> model.typeDefinitions.values().forEach(def -> {
			if (def.getClassName() != null) {
				knownClasses.put(def.getClassName(), def);
			}
		}));
		return knownClasses;
	}

	public void resolveAliases() {
		forEachModel((projectId, model) -> {
			for (final var dataType : model.dataPointDefinitions.values()) {
				for (final MiningDataPointDefinitionWithCustomFetch dataPoint : dataType.values()) {
					model.resolveAliasType(dataPoint, globalModel);
				}
			}
		});
	}

	public Optional<MiningDataTypeDefinition> getTypeDefinition(final String name, final Optional<Long> projectId) {
		MiningDataTypeDefinition type = null;
		final Optional<RegistryModel> projectModel = getProjectModel(projectId);
		if (projectModel.isPresent()) {
			type = projectModel.get().typeDefinitions.get(name);
		}
		if (type == null) {
			type = globalModel.typeDefinitions.get(name);
		}
		return Optional.ofNullable(type);
	}

	public Map<String, MiningDataTypeDefinition> getTypeDefinitions(final Optional<Long> projectId) {
		Map<String, MiningDataTypeDefinition> typeDefs = getModel(Optional.empty()).typeDefinitions;
		final Optional<RegistryModel> projectModel = getProjectModel(projectId);
		if (projectModel.isPresent()) {
			typeDefs = new HashMap<>(typeDefs);
			typeDefs.putAll(projectModel.get().typeDefinitions);
		}
		return typeDefs;
	}

	public Map<String, MiningEnumDefinition> getEnumDefinitions(final Optional<Long> projectId) {
		Map<String, MiningEnumDefinition> enumDefs = getModel(Optional.empty()).enumDefinitions;
		final Optional<RegistryModel> projectModel = getProjectModel(projectId);
		if (projectModel.isPresent()) {
			enumDefs = new HashMap<>(enumDefs);
			enumDefs.putAll(projectModel.get().enumDefinitions);
		}
		return enumDefs;
	}

	public Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> getDataPointDefinitions(final Optional<Long> projectId) {
		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> allDpds;
		final Optional<RegistryModel> projectModel = getProjectModel(projectId);
		if (projectModel.isPresent()) {
			final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dpds = new HashMap<>();
			getModel(Optional.empty()).dataPointDefinitions.entrySet()
					.forEach(e -> dpds.put(e.getKey(), new HashMap<>(e.getValue())));
			projectModel.get().dataPointDefinitions.entrySet()
					.forEach(e -> dpds.computeIfAbsent(e.getKey(), k -> new HashMap<>()).putAll(e.getValue()));
			allDpds = dpds;
		} else {
			allDpds = getModel(Optional.empty()).dataPointDefinitions;
		}
		return allDpds;
	}

	public Map<String, Map<String, MiningDataPointDefinition>> getDataPointDefinitionsWithUsage(final Optional<Long> projectId, final Collection<String> usages) {
		final Map<String, Map<String, MiningDataPointDefinition>> ret = new HashMap<>();
		final Optional<RegistryModel> projectModel = getProjectModel(projectId);
		(projectModel.isPresent()
				? Streams.concat(getModel(Optional.empty()).dataPointDefinitions.entrySet().stream(),
				projectModel.get().dataPointDefinitions.entrySet().stream())
				: getModel(Optional.empty()).dataPointDefinitions.entrySet().stream()
		).forEach(typeEntry -> {
			final Map<String, MiningDataPointDefinition> filteredDataPoints = Optional.ofNullable(ret.get(typeEntry.getKey())).orElse(new HashMap<>());
			for (final Map.Entry<String, MiningDataPointDefinitionWithCustomFetch> dataPointEntry : typeEntry.getValue().entrySet()) {
				/* filter out data points that don't have any of the requested usages */
				if (usages.isEmpty() || !Collections.disjoint(usages, dataPointEntry.getValue().getUsages())) {
					filteredDataPoints.put(dataPointEntry.getKey(), dataPointEntry.getValue());
				}
			}
			if (!filteredDataPoints.isEmpty()) {
				ret.put(typeEntry.getKey(), filteredDataPoints);
			}
		});

		return ret;
	}

	public void validateDataPointTypes() {
		forEachModel((projectId, model) -> model.dataPointDefinitions.values().forEach(entry -> entry.values().forEach(def -> {
			if (!getTypeDefinition(def.getParentTypeName(), projectId).isPresent()) {
				throw new IllegalArgumentException("The type " + def.getParentTypeName()
						+ " for data point " + def.getName() + " provided by " + def.getProvidedBy() + " is not defined.");
			}
		})));
	}

	@Override
	public String toString() {
		final StringBuilder str = new StringBuilder();
		str.append("Queries (" + queryDefinitions.size() + "):\n");
		queryDefinitions.forEach((queryKey, queryDef) -> str.append("\t" + queryKey + " < " + queryDef.getProvidedBy() + "\n"));
		forEachModel((projectId, model) -> {
			str.append((projectId.isPresent() ? "Project " + projectId.get() : "Global") + ":\n");
			str.append("\tEnums (" + model.enumDefinitions.size() + "):\n");
			model.enumDefinitions.forEach((enumKey, enumDef) -> str.append("\t\t" + enumKey + " < " + enumDef.getProvidedBy() + "\n"));
			str.append("\tTypes (" + model.typeDefinitions.size() + "):\n");
			model.typeDefinitions.forEach((typeKey, typeDef) -> str.append("\t\t" + typeKey + " < " + typeDef.getProvidedBy() + "\n"));
			str.append("\tPoints (" + model.dataPointDefinitions.size() + " types):\n");
			model.dataPointDefinitions.forEach((typeKey, dataPoints) -> {
				str.append("\t\t" + typeKey + " (" + dataPoints.size() + " points):\n");
				dataPoints.forEach((pointKey, pointDef) -> {
					str.append("\t\t\t" + pointKey);
					if (!pointDef.getDisplayName().isEmpty()) {
						str.append(" (" + pointDef.getDisplayName() + ")");
					}
					if (pointDef.getScalarType() != null) {
						str.append(" [" + pointDef.getScalarType() + "]");
					}
					if (pointDef.getReferenceTypeName() != null) {
						str.append(" {" + pointDef.getReferenceTypeName() + "}");
					}
					str.append(" < " + pointDef.getProvidedBy() + "\n");
				});
			});
		});
		return str.toString();
	}
}
