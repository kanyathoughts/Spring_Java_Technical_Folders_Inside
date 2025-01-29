/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.entities.SourceContentPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Specifies functions for persisting source code data.
 */
public interface SourceService {
	
	public static final long CONTENT_SIZE_LIMIT = 0x100_0000;  /* 16MiB */
	public static final long CONTENT_SIZE_THRESHOLD_FOR_LIMIT_CHECK = 4000000; /* Threshold to check actual content limit */

	interface SourceOrderBuilder {
		/**
		 * Sorts Sources by their numeric ID.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		SourceOrderBuilder sortNid(SortDirection sort);
		
		/**
		 * Sorts Sources by their name using binary collation.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		SourceOrderBuilder sortName(SortDirection sort);
		
		/**
		 * Sorts Sources by their path using binary collation.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		SourceOrderBuilder sortPath(SortDirection sort);
	}
	
	interface SourceInquiryBuilder extends SourceOrderBuilder {
		
		SourceInquiryBuilder byUid(final UUID id);
		
		SourceInquiryBuilder byNid(final Long id);
		
		/**
		 * Filters a single Source by it's ID.
		 * @param id ID of a Source.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder byId(EntityId id);
		
		/**
		 * Filters Sources by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder ofProject(EntityId projectId);
		
		/**
		 * Determines if the content of a Source should be retrieved along with the meta data.
		 * This overrides content caching.
		 * @param includeContent Whether to load the content immediately.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder includeContent(boolean includeContent);
		
		/**
		 * Specifies a cache for source content.
		 * @param cache Cache for source content by UID.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder usingContentCache(CachingFunction<UUID, BinaryString> cache);
		
		/**
		 * Filters Sources by name.
		 * @param name Pattern matching the names to include.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withName(String name);

		/**
		 * Filters Sources by name.
		 * @param names Pattern matching the names to include.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withNames(Collection<String> names);
		
		/**
		 * Filters Sources by path.
		 * @param path Pattern matching the paths to include.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withPath(String path);
		
		/**
		 * Filters Sources with path matching a (POSIX-style) regular expression.
		 * CAUTION: Regular expressions are not safe for arbitrary user input.
		 * Consider using {@link #withPathGlob(String)} for user parameterized queries.
		 * @param pathRegex Pattern matching the paths to include.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withPathRegex(String pathRegex);
		
		/**
		 * Filters Sources with path matching a pattern.
		 * @param antPattern Pattern to match.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withPathGlob(String antPattern);
		
		/**
		 * Filters Sources by the given numeric IDs.
		 * @param nids Set of IDs to include.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withUids(Collection<UUID> nids);
		
		/**
		 * Filters Sources by the given numeric IDs.
		 * @param nids Set of IDs to include.
		 * @return Filter builder.
		 */
		SourceInquiryBuilder withNids(Collection<Long> nids);
		
		SourceInquiryBuilder withTechnology(Technology technology);
		
		SourceInquiryBuilder withTechnology(Collection<Technology> technology);
		
		SourceInquiryBuilder withType(Type type);
		
		SourceInquiryBuilder withType(Collection<Type> type);
		
		SourceInquiryBuilder withReferenceFrom(EntityId sourceId);
		
		SourceInquiryBuilder withModule(EntityId moduleId);
		
		SourceInquiryBuilder withModuleExists(boolean predicate);
		
		SourceInquiryBuilder withModuleHashDiffers(boolean predicate, final boolean includeNoModule);
		
		<T> SourceInquiryBuilder ofProjectNIDsWithPaths(Iterable<T> values, Function<T, Long> projects, Function<T, String> paths);
	}
	
	UUID put(EntityId project, BinaryString content);
	UUID put(UUID id, @Nullable EntityId project, BinaryString content);
	EntityId create(final SourcePojoPrototype sourceObject);
	EntityId update(final SourcePojoPrototype sourceObject);
	List<SourcePojo> find(BuildingConsumer<SourceInquiryBuilder> builder);
	List<EntityId> findIDs(BuildingConsumer<SourceInquiryBuilder> builder);
	Paged<SourcePojo> find(Pagination paging, BuildingConsumer<SourceInquiryBuilder> builder);
	Optional<SourcePojo> findOne(BuildingConsumer<SourceInquiryBuilder> builder);
	Optional<SourcePojo> findAny(BuildingConsumer<SourceInquiryBuilder> builder);
	SourcePojo get(UUID id);
	SourcePojo get(EntityId id);
	SourcePojo get(UUID id, @Nullable EntityId project);
	SourcePojo get(BuildingConsumer<SourceInquiryBuilder> builder);
	long count(BuildingConsumer<SourceInquiryBuilder> builder);
	long countReferences();
	BinaryString getContent(UUID id);
	BinaryString getContent(UUID id, @Nullable EntityId project);
	Optional<SourceContentPojo> findAnyContent(BuildingConsumer<SourceInquiryBuilder> builder);
	List<SourceContentPojo> findContent(BuildingConsumer<SourceInquiryBuilder> builder);
	List<SourceContentPojo> findContent(Collection<UUID> ids, @Nullable EntityId project);
	Optional<BinaryString> findContent(EntityId id);
	void remove(EntityId id, @Nullable EntityId project);
	int remove(Collection<EntityId> id, @Nullable EntityId project);
	List<UUID> removeAll(BuildingConsumer<SourceInquiryBuilder> builder);
	int putReference(EntityId fromSource, EntityId toSource);
	int removeReferences(EntityId sourceId);
	int removeAllReferences(EntityId projectId);
	
}
