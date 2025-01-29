/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import javax.persistence.EntityNotFoundException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.SourcePgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourceContentPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Central point for accessing and modifying Source entities.
 */
@Service
@Primary
public class SourceServiceImpl implements SourceService {
	
	private final SourcePgDao dao;
	
	@Autowired
	public SourceServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		dao = new SourcePgDao(jdbcTemplate);
	}
	
	@Override
	public Optional<SourcePojo> findOne(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.findOne(builder);
	}
	
	@Override
	public List<SourcePojo> find(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.find(builder);
	}
	
	@Override
	public Paged<SourcePojo> find(final Pagination paging, final BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.find(paging, builder);
	}
	
	@Override
	public List<EntityId> findIDs(BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.findIDs(builder);
	}
	
	@Override
	public Optional<SourcePojo> findAny(BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.findAny(builder);
	}
	
	@Override
	public SourcePojo get(BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.findAny(builder).orElseThrow(() -> new MiningEntityNotFoundException("Source is not present in DB"));
	}
	
	@Override
	public BinaryString getContent(UUID id, @Nullable final EntityId project) {
		return dao.getSource(id, project);
	}

	@Override
	public Optional<SourceContentPojo> findAnyContent(BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.findAnySource(builder);
	}

	@Override
	public List<SourceContentPojo> findContent(BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.findSource(builder);
	}
	
	@Override
	public List<SourceContentPojo> findContent(Collection<UUID> nids, @Nullable final EntityId project) {
		return dao.findSource(q -> {
			q.withUids(nids);
			if (project != null) {
				q.ofProject(project);
			}
		});
	}

	@Override
	public Optional<BinaryString> findContent(EntityId id) {
		return Optional.empty();
	}

	@Override
	public UUID put(final EntityId project, final BinaryString content) {
		return dao.putSource(project, null, content.get());
	}
	
	@Override
	public UUID put(final UUID id, @Nullable final EntityId project, final BinaryString content) {
		if (project == null) {
			dao.putSource(id, content.get());
			return id;
		} else {
			return dao.putSource(project, id, content.get());
		}
	}
	
	@Override
	public EntityId create(final SourcePojoPrototype sourceObject) {
		return dao.put(sourceObject, true);
	}
	
	@Override
	public EntityId update(final SourcePojoPrototype sourceObject) {
		return dao.put(sourceObject, false);
	}
	
	@Override
	public void remove(final EntityId id, @Nullable final EntityId project) {
		dao.remove(id, project);
	}
	
	@Override
	public int remove(final Collection<EntityId> id, @Nullable final EntityId project) {
		return dao.remove(id, project);
	}
	
	@Override
	public List<UUID> removeAll(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.removeAll(builder);
	}
	
	@Override
	public int putReference(final EntityId fromSource, final EntityId toSource) {
		return dao.putReference(fromSource, toSource);
	}

	@Override
	public SourcePojo get(final UUID id) {
		return get(id, null);
	}
	
	@Override
	public SourcePojo get(final EntityId id) {
		return dao.findAny(q -> q.byId(id)).orElseThrow(() -> new EntityNotFoundException("Source " + id + " not found"));
	}

	@Override
	public BinaryString getContent(final UUID id) {
		return getContent(id, null);
	}
	
	@Override
	public SourcePojo get(final UUID id, @Nullable final EntityId project) {
		return dao.findAny(q -> {
			q.byUid(id);
			if (project != null) {
				q.ofProject(project);
			}
		}).orElseThrow(() -> new EntityNotFoundException("Source " + id + " not found" + (project != null ? " in Project " + project.toString() : "")));
	}

	@Override
	public long count(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return dao.count(builder);
	}

	@Override
	public long countReferences() {
		return dao.countReferences();
	}

	@Override
	public int removeReferences(final EntityId sourceId) {
		return dao.removeReferences(sourceId);
	}

	@Override
	public int removeAllReferences(final EntityId projectId) {
		return dao.removeAllReferences(projectId);
	}
	
}
