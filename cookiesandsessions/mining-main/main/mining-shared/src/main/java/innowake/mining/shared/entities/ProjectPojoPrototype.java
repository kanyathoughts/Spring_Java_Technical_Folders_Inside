/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.time.Instant;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.model.ProjectNature;

/**
 * Project entity request class.
 */
public class ProjectPojoPrototype extends MiningSequentialPojoPrototype<ProjectPojoPrototype> {
	public final Definable<EntityId> client = new Definable<>(false, "Project.client");
	public final Definable<String> name = new Definable<>(false, "Project.name");
	
	public final Definable<Long> sourceCodeRevision = new Definable<>(true, "Project.sourceCodeRevision");
	public final Definable<Long> metricsBaseRevision = new Definable<>(true, "Project.metricsBaseRevision");
	public final Definable<String> metricsVersion = new Definable<>(true, "Project.metricsVersion");
	public final Definable<Instant> metricsDate = new Definable<>(true, "Project.metricsDate");
	public final Definable<List<SearchOrder>> searchOrders = new Definable<>(true, "Project.searchOrders");
	public final Definable<Long> defaultTaxonomyCategory = new Definable<>(true, "Project.defaultTaxonomyCategory");
	public final Definable<Long> technicalTaxonomyCategory = new Definable<>(true, "Project.technicalTaxonomyCategory");
	public final Definable<Set<ProjectNature>> natures = new Definable<>(true, "Project.projectNatures");
	public final Definable<String> metaDataBackupId = new Definable<>(true, "Project.metaDataBackupId");
	
	public ProjectPojoPrototype() {
		super("Project");
	}
	
	@JsonAlias("clientId")
	public ProjectPojoPrototype setClient(final EntityId client) {
		this.client.set(this.client.orElseNonNull(EntityId.VOID).merge(client));
		return this;
	}
	
	public ProjectPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
	
	public ProjectPojoPrototype setSourceCodeRevision(@Nullable final Long sourceCodeRevision) {
		this.sourceCodeRevision.set(sourceCodeRevision);
		return this;
	}
	
	public ProjectPojoPrototype setMetricsBaseRevision(@Nullable final Long metricsBaseRevision) {
		this.metricsBaseRevision.set(metricsBaseRevision);
		return this;
	}
	
	public ProjectPojoPrototype setMetricsVersion(@Nullable final String metricsVersion) {
		this.metricsVersion.set(metricsVersion);
		return this;
	}
	
	public ProjectPojoPrototype setMetricsDate(@Nullable final Instant metricsDate) {
		this.metricsDate.set(metricsDate);
		return this;
	}
	
	public ProjectPojoPrototype setSearchOrders(@Nullable final List<SearchOrder> searchOrders) {
		this.searchOrders.set(searchOrders);
		return this;
	}
	
	public ProjectPojoPrototype setDefaultTaxonomyCategory(@Nullable final Long defaultTaxonomyCategory) {
		this.defaultTaxonomyCategory.set(defaultTaxonomyCategory);
		return this;
	}
	
	public ProjectPojoPrototype setTechnicalTaxonomyCategory(@Nullable final Long technicalTaxonomyCategory) {
		this.technicalTaxonomyCategory.set(technicalTaxonomyCategory);
		return this;
	}
	
	public ProjectPojoPrototype setNatures(@Nullable final Set<ProjectNature> projectNatures) {
		this.natures.set(projectNatures);
		return this;
	}

	public ProjectPojoPrototype setMetaDataBackupId(@Nullable final String metaDataBackupId) {
		this.metaDataBackupId.set(metaDataBackupId);
		return this;
	}
	
}
