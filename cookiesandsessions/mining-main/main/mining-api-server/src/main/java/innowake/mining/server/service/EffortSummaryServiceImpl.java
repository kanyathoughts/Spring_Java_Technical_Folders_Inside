/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.postgres.EffortSummaryPgDao;
import innowake.mining.shared.access.EffortSummaryService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.EffortSummaryPojo;
import innowake.mining.shared.entities.EffortSummaryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Central point for accessing and modifying effort summary entities.
 */
@Service
public class EffortSummaryServiceImpl implements EffortSummaryService {

	private final EffortSummaryPgDao dao;

	@Autowired
	public EffortSummaryServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		dao = new EffortSummaryPgDao(jdbcTemplate);
	}

	@Override
	public long create(final EffortSummaryPojoPrototype effortSummary) {
		return dao.put(effortSummary, true);
	}

	@Override
	public void update(final EffortSummaryPojoPrototype effortSummary) {
		dao.put(effortSummary, false);
	}

	@Override
	public int create(final List<EffortSummaryPojoPrototype> effortSummaries) {
		return dao.createBatch(effortSummaries);
	}

	@Override
	public Optional<EffortSummaryPojo> findAny(final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return dao.findAny(builder);
	}
	
	@Override
	public List<EffortSummaryPojo> find(final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return dao.find(builder);
	}

	@Override
	public Paged<EffortSummaryPojo> find(final Pagination paging, final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return dao.find(paging, builder);
	}

	@Override
	public int delete(final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return dao.delete(builder);
	}

}
