/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.entities.SourceContentPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Dummy implementation of the {@link SourceService} for testing without a database.
 */
public class SourceCachingServiceDummy implements SourceCachingService {

	final Collection<SourcePojo> mockSources;
	
	public SourceCachingServiceDummy(final Collection<SourcePojo> mockSources) {
		this.mockSources = mockSources;
	}

	class MockSourceInquiryBuilder implements SourceService.SourceInquiryBuilder {
		
		private Predicate<SourcePojo> filterChain = o -> true; 

		@Override
		public SourceInquiryBuilder byUid(UUID id) {
			filterChain = filterChain.and(o -> id.equals(o.getUid()));
			return this;
		}

		@Override
		public SourceInquiryBuilder byNid(Long id) {
			filterChain = filterChain.and(o -> id.equals(o.getId()));
			return this;
		}
		
		@Override
		public SourceOrderBuilder sortNid(final SortDirection sort) {
			throw new IllegalStateException();
		}

		@Override
		public SourceOrderBuilder sortName(final SortDirection sort) {
			throw new IllegalStateException();
		}

		@Override
		public SourceOrderBuilder sortPath(final SortDirection sort) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder byId(final EntityId id) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder ofProject(final EntityId projectId) {
			filterChain = filterChain.and(o -> projectId.equals(o.getProject()));
			return this;
		}

		@Override
		public SourceInquiryBuilder includeContent(final boolean includeContent) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder usingContentCache(final CachingFunction<UUID, BinaryString> cache) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withName(final String name) {
			filterChain = filterChain.and(o -> o.getName().startsWith(name));
			return this;
		}

		@Override
		public SourceInquiryBuilder withPath(final String path) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withPathRegex(final String pathRegex) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withUids(final Collection<UUID> nids) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withNids(final Collection<Long> nids) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withTechnology(final Technology technology) {
			filterChain = filterChain.and(o -> technology.equals(o.getTechnology()));
			return this;
		}

		@Override
		public SourceInquiryBuilder withTechnology(final Collection<Technology> technology) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withType(final Type type) {
			filterChain = filterChain.and(o -> type.equals(o.getType()));
			return this;
		}

		@Override
		public SourceInquiryBuilder withType(final Collection<Type> type) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withReferenceFrom(final EntityId sourceId) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withModuleExists(final boolean predicate) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withModuleHashDiffers(final boolean predicate, final boolean includeNoModule) {
			throw new IllegalStateException();
		}

		@Override
		public <T> SourceInquiryBuilder ofProjectNIDsWithPaths(final Iterable<T> values, final Function<T, Long> projects, final Function<T, String> paths) {
			throw new IllegalStateException();
		}
		
		public Stream<SourcePojo> build() {
			return mockSources.stream().filter(filterChain);
		}

		@Override
		public SourceInquiryBuilder withPathGlob(final String antPattern) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withModule(final EntityId moduleId) {
			throw new IllegalStateException();
		}

		@Override
		public SourceInquiryBuilder withNames(Collection<String> names) {
			throw new IllegalStateException();
		}
	}
	
	@Override
	public List<UUID> removeAll(final BuildingConsumer<SourceInquiryBuilder> project) {
		throw new IllegalStateException();
	}
	
	@Override
	public int remove(final Collection<EntityId> id, @Nullable final EntityId project) {
		throw new IllegalStateException();
	}
	
	@Override
	public void remove(final EntityId id, @Nullable final EntityId project) {
		throw new IllegalStateException();
	}
	
	@Override
	public int putReference(final EntityId fromSource, final EntityId toSource) {
		throw new IllegalStateException();
	}
	
	@Override
	public EntityId create(final SourcePojoPrototype sourceObject) {
		throw new IllegalStateException();
	}
	
	@Override
	public EntityId update(final SourcePojoPrototype sourceObject) {
		throw new IllegalStateException();
	}
	
	@Override
	public UUID put(final UUID id, @Nullable final EntityId project, final BinaryString content) {
		throw new IllegalStateException();
	}
	
	@Override
	public UUID put(final EntityId project, final BinaryString content) {
		throw new IllegalStateException();
	}
	
	@Override
	public BinaryString getContent(final UUID id, @Nullable final EntityId project) {
		throw new IllegalStateException();
	}
	
	@Override
	public BinaryString getContent(final UUID id) {
		throw new IllegalStateException();
	}

	@Override
	public SourcePojo get(final EntityId id) {
		throw new IllegalStateException();
	}
	
	@Override
	public SourcePojo get(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}
	
	@Override
	public SourcePojo get(final UUID id, @Nullable final EntityId project) {
		throw new IllegalStateException();
	}
	
	@Override
	public SourcePojo get(final UUID id) {
		throw new IllegalStateException();
	}
	
	@Override
	public Optional<SourcePojo> findOne(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}
	
	@Override
	public List<EntityId> findIDs(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}
	
	@Override
	public Optional<BinaryString> findContent(final EntityId id) {
		throw new IllegalStateException();
	}

	@Override
	public List<SourceContentPojo> findContent(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}

	@Override
	public Optional<SourceContentPojo> findAnyContent(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}

	@Override
	public List<SourceContentPojo> findContent(final Collection<UUID> ids, @Nullable final EntityId project) {
		throw new IllegalStateException();
	}
	
	@Override
	public Optional<SourcePojo> findAny(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}
	
	@Override
	public Paged<SourcePojo> find(final Pagination paging, final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}
	
	@Override
	public List<SourcePojo> find(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return builder.prepare(new MockSourceInquiryBuilder()).build().collect(Collectors.toList());
	}
	
	@Override
	public void resetCaches() {
	}

	@Override
	public void invalidateCaches(Collection<EntityId> sourceIds) {
	}

	@Override
	public SourcePojo cachingByProjectPath(final Long projectId, final String path) {
		throw new IllegalStateException();
	}

	@Override
	public Collection<SourcePojo> cachingByProjectPath(final Collection<Tuple2<Long, String>> sourcesByProjectAndPath) {
		throw new IllegalStateException();
	}

	@Override
	public long count(final BuildingConsumer<SourceInquiryBuilder> builder) {
		throw new IllegalStateException();
	}
	
	@Override
	public long countReferences() {
		throw new IllegalStateException();
	}

	@Override
	public int removeReferences(EntityId sourceId) {
		throw new IllegalStateException();
	}

	@Override
	public int removeAllReferences(EntityId projectId) {
		throw new IllegalStateException();
	}
}
