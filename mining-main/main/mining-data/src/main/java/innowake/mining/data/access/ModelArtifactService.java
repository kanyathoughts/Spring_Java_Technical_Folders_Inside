/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Functions for accessing module and module related database entities.
 * <p>This service should be used for the discovery only when instances of {@code ModelArtifact} or {@code ModelStatement} are required. For all other cases
 * use the {@link ModuleService} instead.</p>
 */
public interface ModelArtifactService {

	/**
	 * Query builder for performing queries on {@code module} entities.
	 */
	interface ModelArtifactInquiryBuilder {

		/**
		 * Filters modules by a project
		 * 
		 * @param project ID of the project
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters modules by {@code source}
		 * 
		 * @param source the {@link EntityId} of the source
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder ofSource(EntityId source);

		/**
		 * Filters modules by numeric or unique id
		 * 
		 * @param module {@linkplain EntityId} of the module
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder byId(EntityId module);

		/**
		 * Filters modules by unique id
		 * 
		 * @param uid unique module id
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder byUid(UUID uid);

		/**
		 * Filters modules by unique ids
		 * 
		 * @param uids unique module ids
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder byUids(Collection<UUID> uids);

		/**
		 * Filters modules by numeric id
		 * 
		 * @param nid numeric module id
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder byNid(Long nid);

		/**
		 * Filters modules by numeric ids
		 * 
		 * @param nids numeric module ids
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder byNids(Collection<Long> nids);

		/**
		 * Filters modules by link hash
		 * 
		 * @param linkHash the link hash
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withLinkHash(String linkHash);

		/**
		 * Filters modules by technology
		 * 
		 * @param technology the technology
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withTechnology(Technology technology);

		/**
		 * Filters modules by technologies
		 * 
		 * @param technologies the technologies
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withTechnologies(Collection<Technology> technologies);

		/**
		 * Filters modules by type
		 * 
		 * @param type the type
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withType(Type type);

		/**
		 * Filters modules by types
		 * 
		 * @param types the types
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withTypes(Collection<Type> types);

		/**
		 * Filters modules by technologies and types. 
		 * 
		 * @param technologiesAndTypes {@link Tuple2 tuples} of {@link Technology} and {@link Type}
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withTechnologiesAndTypes(Collection<Tuple2<Technology, Type>> technologiesAndTypes);

		/**
		 * Filters modules by source references.
		 * 
		 * @param moduleId the id of the source module whose referencing modules must be filtered
		 * @param type the type if the module references
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withSourceRelationshipsFrom(EntityId moduleId, RelationshipType type);

		/**
		 * Filters modules by destination references.
		 * 
		 * @param moduleId the id of the destination module whose referencing modules must be filtered
		 * @param type the type if the module references
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withDestinationRelationshipsTo(EntityId moduleId, RelationshipType type);

		/**
		 * Filters modules by path
		 * 
		 * @param path the module path
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withPath(String path);

		/**
		 * Filters modules by path
		 * 
		 * @param path the module path
		 * @param caseInsensitive {@code true} to match the path case insensitive. {@code false} to match case sensitive (default)
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withPath(String path, boolean caseInsensitive);

		/**
		 * Filters modules by path. A module matches when its own path or the path of its containing module matches with the given {@code path}.
		 * 
		 * @param path the module path
		 * @param caseInsensitive {@code true} to match the path case insensitive. {@code false} to match case sensitive (default)
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withPathsSelfOrContaining(String path, boolean caseInsensitive);

		/**
		 * Filters modules by name
		 * 
		 * @param name the module name
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withName(String name);

		/**
		 * Filters modules by name
		 * 
		 * @param name the module name
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withNameLike(String name);

		/**
		 * Filters modules by name
		 * 
		 * @param name the module name
		 * @param caseInsensitive {@code true} to match the name case insensitive. {@code false} to match case sensitive (default)
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withName(String name, boolean caseInsensitive);

		/**
		 * Filters modules by names
		 * 
		 * @param names the module names
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withNames(Collection<String> names);

		/**
		 * Filters modules by description
		 * 
		 * @param description the module description
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withDescription(String description);

		/**
		 * Filters modules by identified flag
		 * 
		 * @param identified the module identified flag
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withIdentified(boolean identified);

		/**
		 * Filters modules by representation
		 * 
		 * @param representation the type of representation
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withRepresentation(Representation representation);

		/**
		 * Filters modules by metrics_date
		 * 
		 * @param metricsDate the module metrics_date
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withMetricsDate(Instant metricsDate);

		/**
		 * Filters modules by creator
		 * 
		 * @param creator the module creator
		 * @return this instance for method chaining
		 */
		ModelArtifactInquiryBuilder withCreator(Creator creator);
	}

	/**
	 * Returns all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModelArtifactInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of matching {@linkplain LazyModelArtifact LazyModelArtifacts}
	 */
	List<LazyModelArtifact> find(BuildingConsumer<ModelArtifactInquiryBuilder> builder);

	/**
	 * Returns the first {@code module} entity that matches with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModelArtifactInquiryBuilder} containing the filter criteria and sort options.
	 * @return Optional containing the first matching LazyModelArtifact or empty if non matched
	 */
	Optional<LazyModelArtifact> findAny(BuildingConsumer<ModelArtifactInquiryBuilder> builder);
}
