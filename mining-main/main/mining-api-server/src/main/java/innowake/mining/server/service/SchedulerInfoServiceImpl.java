/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.shared.entities.scheduler.SchedulerImportPojo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.postgres.SchedulerInfoPgDao;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Implementation of the {@link SchedulerInfoService}
 */
@Service
public class SchedulerInfoServiceImpl implements SchedulerInfoService {

	private final SchedulerInfoPgDao schedulerInfoPgDao;

	@Autowired
	public SchedulerInfoServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		this.schedulerInfoPgDao = new SchedulerInfoPgDao(jdbcTemplate);
	}

	@Override
	public UUID createSchedulerImport(final SchedulerImportPojoPrototype schedulerImport) {
		return schedulerInfoPgDao.createSchedulerImport(schedulerImport);
	}

	@Override
	public UUID createSchedulerEntry(final SchedulerEntryPojoPrototype schedulerEntry) {
		return schedulerInfoPgDao.createSchedulerEntry(schedulerEntry);
	}

	@Override
	public Paged<SchedulerImportPojo> findImports(final Pagination paging, final BuildingConsumer<SchedulerImportInquiryBuilder> builder) {
		return schedulerInfoPgDao.findSchedulerImports(paging, builder);
	}

	@Override
	public List<UUID> createSchedulerEntryRelationships(final List<SchedulerEntryRelationshipPojoPrototype> schedulerEntries) {
		return schedulerInfoPgDao.createSchedulerEntryRelationships(schedulerEntries);
	}

	@Override
	public Paged<SchedulerEntryRelationshipPojo> findRelationships(final Pagination paging,
			final BuildingConsumer<SchedulerEntryRelationshipInquireBuilder> builder) {
		return schedulerInfoPgDao.findRelationships(paging, builder);
	}

	@Override
	public Optional<SchedulerEntryPojo> findAnyEntry(final BuildingConsumer<SchedulerEntryInquiryBuilder> builder) {
		return schedulerInfoPgDao.findAnySchedulerEntry(builder);
	}

	@Override
	public int deleteSchedulerImport(final BuildingConsumer<SchedulerImportInquiryBuilder> builder) {
		return schedulerInfoPgDao.deleteSchedulerImport(builder);
	}

	@Override
	public Paged<SchedulerEntryPojo> findEntries(final Pagination paging, final BuildingConsumer<SchedulerEntryInquiryBuilder> builder) {
		return schedulerInfoPgDao.findSchedulerEntries(paging, builder);
	}

	@Override
	public List<SchedulerEntryPojo> findEntries(final BuildingConsumer<SchedulerEntryInquiryBuilder> builder) {
		return schedulerInfoPgDao.findSchedulerEntries(builder);
	}
}
