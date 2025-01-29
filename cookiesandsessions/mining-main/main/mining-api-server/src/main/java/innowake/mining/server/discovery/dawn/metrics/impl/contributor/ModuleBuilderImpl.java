/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.contributor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelDeadCode;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.AdditionalInfo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.cobol.parser.ast.statement.CobolEnterStmt.LanguageType;

/**
 * Implementation for {@link DiscoveryBuilder.ModuleBuilder}.
 */
class ModuleBuilderImpl implements DiscoveryBuilder.AnchorToBuilder {

	final DiscoveryBuilderImpl parentBuilder;

	final String contributorClassName;
	final ContributorResult.Type type;
	@Nullable
	final ModuleFilter moduleFilter;
	final Set<ResolutionFlag> resolutionFlags;
	@Nullable
	String name;
	@Nullable
	ModuleType moduleType;
	@Nullable
	Identification identification;
	@Nullable
	final ModuleLocation location;
	@Nullable
	Origin origin;

	final List<AdditionalInfo> additionalInfos = new ArrayList<>();
	final List<ErrorMarker> errors = new ArrayList<>();
	final List<ModelDeadCode> deadCodes = new ArrayList<>();
	final List<DeferredActionDefinition> deferredActions = new ArrayList<>();
	final List<StatementBuilderImpl> statementBuilders = new ArrayList<>();
	final List<DependencyBuilderImpl> dependencyBuilders = new ArrayList<>();

	/**
	 * Creates a module builder as required for {@link DiscoveryBuilder#declareExternalModule(String, ModuleType)}.
	 * @param contributorClassName name of the contributor class that uses this builder
	 * @param name name of the external module
	 * @param moduleType type of the external module
	 * @param resolutionFlags resolution flags to use when storing the module
	 * @return a new module builder
	 */
	static ModuleBuilderImpl forExternalModule(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName, final String name,
			final ModuleType moduleType, final ResolutionFlag... resolutionFlags) {
		return new ModuleBuilderImpl(parentBuilder, contributorClassName, ContributorResult.Type.EXTERNAL_MODULE, null,
				Arrays.asList(resolutionFlags).isEmpty() ? EnumSet.noneOf(ResolutionFlag.class) : EnumSet.copyOf(Arrays.asList(resolutionFlags)), name,
				moduleType, null);
	}
	
	/**
	 * Creates a module builder as required for {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, Origin)}.
	 * @param contributorClassName name of the contributor class that uses this builder
	 * @param name name of the external module
	 * @param moduleType type of the external module
	 * @param origin {@link Origin} of the external module
	 * @param resolutionFlags resolution flags to use when storing the module
	 * @return a new module builder
	 */
	static ModuleBuilderImpl forExternalModule(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName, final String name,
			final ModuleType moduleType, final Origin origin, final ResolutionFlag... resolutionFlags) {
		return new ModuleBuilderImpl(parentBuilder, contributorClassName, ContributorResult.Type.EXTERNAL_MODULE, null,
				Arrays.asList(resolutionFlags).isEmpty() ? EnumSet.noneOf(ResolutionFlag.class) : EnumSet.copyOf(Arrays.asList(resolutionFlags)), name,
				moduleType, null, origin);
	}

	/**
	 * Creates a module builder as required for {@link DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...)}.
	 * @param contributorClassName name of the contributor class that uses this builder
	 * @param moduleFilter the filter used to locate the anchor module
	 * @param resolutionFlags resolution flags to use when locating the anchor module
	 * @return a new module builder
	 */
	static ModuleBuilderImpl forAnchorTo(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName,
			final ModuleFilter moduleFilter, final ResolutionFlag... resolutionFlags) {
		return new ModuleBuilderImpl(parentBuilder, contributorClassName, ContributorResult.Type.ANCHOR_TO, moduleFilter,
				Arrays.asList(resolutionFlags).isEmpty() ? EnumSet.noneOf(ResolutionFlag.class) : EnumSet.copyOf(Arrays.asList(resolutionFlags)), null, null,
				null);
	}

	/**
	 * Creates a module builder as required for {@link DiscoveryBuilderFromSource#declareRootModule(String, LanguageType)}.
	 * @param contributorClassName name of the contributor class that uses this builder
	 * @param name name of the root module
	 * @param moduleType type of the root module
	 * @return a new module builder
	 */
	static ModuleBuilderImpl forRootModule(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName,
			final String name, final ModuleType languageType) {
		return new ModuleBuilderImpl(parentBuilder, contributorClassName, ContributorResult.Type.ROOT_MODULE, null, Collections.emptySet(),
				name, languageType, null);
	}

	/**
	 * Creates a module builder as required for {@link DiscoveryBuilderFromSource#declareSubModule(String, LanguageType, ModuleLocation)}.
	 * @param contributorClassName name of the contributor class that uses this builder
	 * @param name name of the root module
	 * @param moduleType type of the root module
	 * @param location the location of the sub-module inside of the parent module
	 * @return a new module builder
	 */
	static ModuleBuilderImpl forSubModule(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName,
			final String name, final ModuleType moduleType, @Nullable final ModuleLocation location) {
		return new ModuleBuilderImpl(parentBuilder, contributorClassName, ContributorResult.Type.SUB_MODULE, null, Collections.emptySet(),
				name, moduleType, location);
	}

	private ModuleBuilderImpl(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName, final ContributorResult.Type type,
			@Nullable final ModuleFilter moduleFilter, final Set<ResolutionFlag> resolutionFlags,
			@Nullable final String name, @Nullable final ModuleType moduleType,
			@Nullable final ModuleLocation location) {
		this.parentBuilder = parentBuilder;
		this.contributorClassName = contributorClassName;
		this.type = type;
		this.moduleFilter = moduleFilter;
		this.resolutionFlags = resolutionFlags;
		this.name = name;
		this.moduleType = moduleType;
		this.location = location;
	}
	
	private ModuleBuilderImpl(final DiscoveryBuilderImpl parentBuilder, final String contributorClassName, final ContributorResult.Type type,
			@Nullable final ModuleFilter moduleFilter, final Set<ResolutionFlag> resolutionFlags,
			@Nullable final String name, @Nullable final ModuleType moduleType,
			@Nullable final ModuleLocation location, @Nullable final Origin origin) {
		this(parentBuilder, contributorClassName, type, moduleFilter, resolutionFlags, name, moduleType, location);
		this.origin = origin;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder addAdditionalInfo(final AdditionalInfo additionalInfo) {
		additionalInfos.add(additionalInfo);
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder addError(final Severity severity, final ErrorKey key, final String message) {
		errors.add(new ErrorMarker(severity, key, message, null));
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder addError(final Severity severity, final ErrorKey key, final String message, final Throwable cause) {
		errors.add(new ErrorMarker(severity, key, message, null));
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder addError(final Severity severity, final ErrorKey key, final String message, final AstNodeLocation location) {
		errors.add(new ErrorMarker(severity, key, message, location));
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder addError(final Severity severity, final ErrorKey key, final String message, final Throwable cause,
			final AstNodeLocation location) {
		errors.add(new ErrorMarker(severity, key, message, location));
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder addDeadCode(final String label, final int line, final int numberOfLines) {
		deadCodes.add(new ModelDeadCode(label, line, numberOfLines));
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder deferAction(final String name) {
		deferredActions.add(new DeferredActionDefinition(contributorClassName, name));
		return this;
	}

	@Override
	public DiscoveryBuilder.ModuleBuilder deferAction(final String name, final Serializable context) {
		deferredActions.add(new DeferredActionDefinition(contributorClassName, name, context));
		return this;
	}

	@Override
	public DiscoveryBuilder.StatementBuilder declareStatement(final StatementType statementType) {
		final StatementBuilderImpl statementBuilder = new StatementBuilderImpl(statementType);
		statementBuilders.add(statementBuilder);
		return statementBuilder;
	}

	@Override
	public DependencyBuilder declareDependency(final RelationshipType relationship, final ModuleFilter filter, ResolutionFlag... flags) {
		final DependencyBuilderImpl dependencyBuilder = new DependencyBuilderImpl(this, relationship, filter, flags);
		dependencyBuilders.add(dependencyBuilder);
		return dependencyBuilder;
	}

	@Override
	public DependencyBuilder declareDependency(final RelationshipType relationship, final List<ModuleFilter> filters, ResolutionFlag... flags) {
		final DependencyBuilderImpl dependencyBuilder = new DependencyBuilderImpl(this, relationship, filters, flags);
		dependencyBuilders.add(dependencyBuilder);
		return dependencyBuilder;
	}

	@Override
	public DependencyBuilder declareDependency(final RelationshipType relationship, final ModuleBuilder module) {
		final DependencyBuilderImpl dependencyBuilder = new DependencyBuilderImpl(this, relationship, (ModuleBuilderImpl) module);
		dependencyBuilders.add(dependencyBuilder);
		return dependencyBuilder;
	}

	@Override
	public ModuleBuilder createIfMissing(final String name, final ModuleType type) {
		this.name = name;
		this.moduleType = type;
		resolutionFlags.add(ResolutionFlag.CREATE_IF_MISSING);
		return this;
	}

	@Override
	public ModuleBuilder createIfMissing(final String name, final ModuleType type, final Identification identification) {
		this.identification = identification;
		return createIfMissing(name, type);
	}
	
	@Override
	public ModuleBuilder createIfMissing(final String name, final ModuleType type, final Origin origin) {
		this.origin = origin;
		if (origin == Origin.ENVIRONMENT) {
			/*
			 * Utility modules should have positive or zero as the source metrics, so handled here.
			 */
			final var sourceMetrics = new SourceMetrics()
					.setCodeLines(Integer.valueOf(0))
					.setCommentLines(Integer.valueOf(0))
					.setComplexityMcCabe(Integer.valueOf(0))
					.setDeadCodeLines(Integer.valueOf(0))
					.setPhysicalLines(Integer.valueOf(0));
			addAdditionalInfo(sourceMetrics);
		}
		return createIfMissing(name, type);
	}

	@Override
	public ModuleBuilder andResetType(final ModuleType type) {
		this.moduleType = type;
		return this;
	}

}
