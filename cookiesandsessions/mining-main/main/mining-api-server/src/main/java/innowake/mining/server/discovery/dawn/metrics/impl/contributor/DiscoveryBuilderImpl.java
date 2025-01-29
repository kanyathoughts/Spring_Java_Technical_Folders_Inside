/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.contributor;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import innowake.lib.core.lang.Nullable;
import innowake.mining.data.model.discovery.ModelDeadCode;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.AdditionalInfo;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleMetadata;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Implementation for {@link DiscoveryBuilder} and {@link DiscoveryBuilderFromSource}.
 * <p>
 * Collects all declarations, then allows to build a list of {@link ContributorResult} via {@link #buildResults()};
 */
public class DiscoveryBuilderImpl implements DiscoveryBuilderFromSource {

	private final String contributorClassName;
	@Nullable
	private final String sourcePath;
	private final List<ModuleBuilderImpl> moduleBuilders = new ArrayList<>();
	@Nullable
	private ModuleBuilderImpl parentModule;

	/**
	 * Creates a new Discovery Builder. This constructor is to be used when the builder is used for the {@link DiscoveryBuilder} interface only.
	 *
	 * @param contributorClassName the class name of the contributor for which this builder is created
	 */
	public DiscoveryBuilderImpl(final String contributorClassName) {
		this.contributorClassName = contributorClassName;
		this.sourcePath = null;
	}

	/**
	 * Creates a new Discovery Builder. This constructor is to be used when the builder is used for the {@link DiscoveryBuilderFromSource} interface.
	 *
	 * @param contributorClassName the class name of the contributor for which this builder is created
	 * @param sourcePath the path of the source object on which the contributor operates (only for DiscoveryBuilderFromSource)
	 */
	public DiscoveryBuilderImpl(final String contributorClassName, @Nullable final String sourcePath) {
		this.contributorClassName = contributorClassName;
		this.sourcePath = sourcePath;
	}

	@Override
	public ModuleBuilder declareExternalModule(final String name, final ModuleType type, final ResolutionFlag... flags) {
		final ModuleBuilderImpl moduleBuilder = ModuleBuilderImpl.forExternalModule(this, contributorClassName, name, type, flags);
		moduleBuilders.add(moduleBuilder);
		return moduleBuilder;
	}
	
	@Override
	public ModuleBuilder declareExternalModule(final String name, final ModuleType type, final Origin origin, final ResolutionFlag... flags) {
		final ModuleBuilderImpl moduleBuilder = ModuleBuilderImpl.forExternalModule(this, contributorClassName, name, type, origin, flags);
		moduleBuilders.add(moduleBuilder);
		return moduleBuilder;
	}

	@Override
	public AnchorToBuilder anchorTo(final ModuleFilter filter, final ResolutionFlag... flags) {
		final ModuleBuilderImpl moduleBuilder = ModuleBuilderImpl.forAnchorTo(this, contributorClassName, filter, flags);
		moduleBuilders.add(moduleBuilder);
		return moduleBuilder;
	}

	@Override
	public ModuleBuilder declareRootModule(final String name, final ModuleType type) {
		final ModuleBuilderImpl moduleBuilder = ModuleBuilderImpl.forRootModule(this, contributorClassName, name, type);
		moduleBuilders.add(moduleBuilder);
		if (parentModule == null) {
			parentModule = moduleBuilder;
		}

		return moduleBuilder;
	}

	@Override
	public ModuleBuilder declareSubModule(final String name, final ModuleType type) {
		final ModuleBuilderImpl moduleBuilder = ModuleBuilderImpl.forSubModule(this, contributorClassName, name, type, null);
		moduleBuilders.add(moduleBuilder);
		return moduleBuilder;
	}

	@Override
	public ModuleBuilder declareSubModule(final String name, final ModuleType type, final ModuleLocation location) {
		final ModuleBuilderImpl moduleBuilder = ModuleBuilderImpl.forSubModule(this, contributorClassName, name, type, location);
		moduleBuilders.add(moduleBuilder);
		return moduleBuilder;
	}

	/**
	 * Builds a list of contributor results from the declarations that were made through this builder.
	 *
	 * @return a list of contributor results
	 */
	public List<ContributorResult> buildResults() {
		return moduleBuilders.stream().filter(this::isValidModuleBuilder).map(builder -> {
			final ModuleFilter moduleFilter = buildModuleFilter(builder);

			final ModulePojoPrototype moduleDefinition = buildModulePojoPrototype(builder);

			final var statements = builder.statementBuilders.stream()
					.map(this::buildStatementPojoPrototype)
					.collect(Collectors.toList());
			final var dependencies = builder.dependencyBuilders.stream()
					.filter(dependencyBuilder -> isValidDependencyBuilder(dependencyBuilder, builder))
					.map(this::buildDependencyDefinition)
					.collect(Collectors.toList());
			final var deadCodes = builder.deadCodes.stream()
					.map(ModelDeadCode::convertToPojoPrototype)
					.collect(Collectors.toList());
			/* Can not convert ErrorMarker to ErrorMarkerPojoPrototypes here cause isValidModuleBuilder() can add errors to parent module */
			return new ContributorResult(builder.type, moduleFilter, builder.resolutionFlags, moduleDefinition, builder.errors, deadCodes,
					builder.deferredActions, statements, dependencies);
		}).collect(Collectors.toList());
	}

	private ModuleFilter buildModuleFilter(final ModuleBuilderImpl builder) {
		switch (builder.type) {
			case ANCHOR_TO:
				return assertNotNull(builder.moduleFilter);
			case EXTERNAL_MODULE:
				return buildModuleFilterForExternalModule(builder);
			case ROOT_MODULE:
				return buildModuleFilterForRootModule(builder);
			case SUB_MODULE:
				return buildModuleFilterForSubModule(builder);
			default:
				throw new IllegalStateException("Unknown module builder type:" + builder.type);
		}
	}

	private ModuleFilter buildModuleFilterForExternalModule(final ModuleBuilderImpl builder) {
		return new ModuleFilter()
				.setNames(assertNotNull(builder.name))
				.setTypes(assertNotNull(builder.moduleType));
	}

	private ModuleFilter buildModuleFilterForRootModule(final ModuleBuilderImpl builder) {
		return new ModuleFilter()
				.setNames(assertNotNull(builder.name))
				.setTypes(assertNotNull(builder.moduleType))
				.setPaths(assertNotNull(sourcePath));
	}

	private ModuleFilter buildModuleFilterForSubModule(final ModuleBuilderImpl builder) {
		if (this.parentModule == null) {
			throw new IllegalStateException("Sub module" + builder.name + " was declared but no root module was declared.");
		}

		return new ModuleFilter()
				.setNames(assertNotNull(builder.name))
				.setTypes(assertNotNull(builder.moduleType))
				.setContainedIn(buildModuleFilterForRootModule(assertNotNull(parentModule)));
	}

	private ModulePojoPrototype buildModulePojoPrototype(final ModuleBuilderImpl builder) {
		final var module = new ModulePojoPrototype();
		/* Default values for Origin and Storage is set below */
		Origin origin = builder.origin == null ? Origin.CUSTOM : builder.origin;
		final ModuleType languageType = builder.moduleType;
		Storage storage;
		if (languageType != null) {
			storage = Storage.from(languageType.getTechnology(), languageType.getType());
			module.setTechnology(languageType.getTechnology());
			module.setType(languageType.getType());
		} else {
			storage = Storage.UNDEFINED;
		}

		if (builder.type == ContributorResult.Type.ROOT_MODULE) {
			module.setPath(sourcePath);
			module.setRepresentation(Representation.PHYSICAL);
			module.setIdentification(Identification.IDENTIFIED);
		} else if (builder.type == ContributorResult.Type.ANCHOR_TO) {
			if (builder.resolutionFlags.contains(ResolutionFlag.CREATE_IF_MISSING) && builder.identification != null) {
				module.setIdentification(builder.identification);
			}
			/* To handle special anchor scenarios - orCreateIfMissing() and andResetType(), which always require an update and the default values of
			 * Storage and Origin need not to be overridden */
			if (languageType == null) {
				storage = null;
				origin = builder.origin;
			}
		} else {
			module.setRepresentation(Representation.VIRTUAL);
			module.setIdentification(Identification.IDENTIFIED);
		}

		if (builder.name != null) {
			module.setName(builder.name);
		}
		if (builder.location != null) {
			module.setLocation(builder.location);
		}
		if (storage != null) {
			module.setStorage(storage);
		}
		if (origin != null) {
			module.setOrigin(origin);
		}
		
		final List<AdditionalInfo> additionalInfos = builder.additionalInfos;
		if (additionalInfos.size() > 2) {
			throw new UnsupportedOperationException("Only two AdditionalInfo of types SourceMetrics and ModuleMetadata is supported for modules");
		}
		for (final var additionalInfo : additionalInfos) {
			if (additionalInfo instanceof SourceMetrics) {
				final var sourceMetrics = ((SourceMetrics) additionalInfo).convertToPojoPrototype();
				final int deadCodes = builder.deadCodes.size();
				if (deadCodes > 0) {
					sourceMetrics.setDeadCodeLines(deadCodes);
				}
				module.setSourceMetrics(sourceMetrics);
			} else if (additionalInfo instanceof ModuleMetadata) {
				module.setInfo(((ModuleMetadata) additionalInfo).getMetadata());
			} else {
				throw new UnsupportedOperationException("Only two AdditionalInfo of types SourceMetrics and ModuleMetadata is supported for modules");
			}
		}

		return module;
	}

	private StatementPojoPrototype buildStatementPojoPrototype(final StatementBuilderImpl builder) {
		final var statement = new StatementPojoPrototype()
				.setProperties(builder.properties)
				.setText(builder.text)
				.setType(builder.statementType);

		if (builder.technology != null) {
			statement.setTechnology(builder.technology);
		}

		return statement;
	}

	private DependencyDefinitionPojoPrototype buildDependencyDefinition(final DependencyBuilderImpl builder) {
		final var dependencyTarget = builder.targetBuilder != null
				? Collections.singletonList(buildModuleFilter(builder.targetBuilder))
				: builder.targets;

		final Map<String, Object> attributes = builder.attributes.isEmpty() ? Collections.emptyMap() : 
			builder.attributes.entrySet().stream().collect(Collectors.toMap(e -> e.getKey().name(), e -> e.getValue()));

		final DependencyDefinitionPojoPrototype prototype = new DependencyDefinitionPojoPrototype()
				.setModuleFilters(dependencyTarget)
				.setResolutionFlags(builder.flags)
				.setRelationshipType(builder.relationship)
				.setBindingType(builder.binding)
				.setAttributes(attributes);
		if (builder.reachedFromModuleBuilder != null) {
			prototype.setReachedFromModules(Collections.singletonList(buildModuleFilter(builder.reachedFromModuleBuilder)));
		} else {
			prototype.setReachedFromModules(builder.reachedFromModules);
		}

		if (builder.location != null) {
			prototype.setLocation(builder.location);
		}

		return prototype;
	}

	private boolean isValidModuleBuilder(final ModuleBuilderImpl builder) {
		final String moduleName = builder.name;

		/* A module name cannot be blank for root module, throw an exception if it is empty*/
		if (StringUtils.isBlank(moduleName) && builder.type == ContributorResult.Type.ROOT_MODULE) {
			throw new IllegalStateException("Name cannot be blank for the declared root module with sourcePath: " + sourcePath );
		} else if (StringUtils.isBlank(moduleName) && builder.type != ContributorResult.Type.ANCHOR_TO) {
			/* Add an error to the root module that a sub-module/external module with blank name as declared and was ignored */
			final var message = String.format("An %s with empty name of type %s was declared and was ignored", builder.type, builder.moduleType);
			if (parentModule != null) {
				parentModule.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, message);
			}
			return false;
		}
		return true; 
	}

	private boolean isValidDependencyBuilder(final DependencyBuilderImpl dependencyBuilder, final ModuleBuilderImpl moduleBuilder) {
		/* Name must be present when targetBuilder is present and when a targetFilter is present, then a name should be present at least in one filter, in-case
		 an moduleId is not mentioned */
		final Predicate<List<ModuleFilter>> haveEmptyNames = targets -> !targets.isEmpty()
				&& targets.stream().allMatch(target -> target.getModuleIds().isEmpty() && target.getNames().contains(StringUtils.EMPTY));

		if ((dependencyBuilder.targetBuilder != null && (StringUtils.isBlank(assertNotNull(dependencyBuilder.targetBuilder).name)
				&& (assertNotNull(dependencyBuilder.targetBuilder).moduleFilter != null
				&& assertNotNull(assertNotNull(dependencyBuilder.targetBuilder).moduleFilter).getNames().contains(StringUtils.EMPTY))))
				|| haveEmptyNames.test(dependencyBuilder.targets)) {
			/* Add an error to the root module that a dependency module with blank name as declared and was ignored */
			final var message = "An dependency with empty name was declared and was ignored";
			moduleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, message);
			return false;
		}
		return true;
	}
}
