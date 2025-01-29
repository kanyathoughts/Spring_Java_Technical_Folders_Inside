/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.postgres.JobInfoPgDao;
import innowake.mining.data.access.postgres.MiningJobInfoPgDao;
import innowake.mining.shared.access.MiningJobInfoService;
import innowake.mining.shared.entities.MiningJobInfoPojo;
import innowake.mining.shared.entities.MiningJobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Central point for accessing and modifying {@code mining_job_info} entities.
 */
@Service
public class MiningJobInfoServiceImpl implements MiningJobInfoService {
	
	private final MiningJobInfoPgDao miningJobInfoDao;
	private final JobInfoPgDao jobInfoDao;
	
	@Autowired
	public MiningJobInfoServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		miningJobInfoDao = new MiningJobInfoPgDao(jdbcTemplate);
		jobInfoDao = new JobInfoPgDao(jdbcTemplate);
	}

	@Override
	public Long count(final BuildingConsumer<MiningJobInfoInquiryBuilder> builder) {
		return miningJobInfoDao.count(builder);
	}
	
	@Override
	public Optional<MiningJobInfoPojo> findAny(final BuildingConsumer<MiningJobInfoInquiryBuilder> builder) {
		final List<MiningJobInfoPojo> jobs = miningJobInfoDao.find(builder);
		return jobs.isEmpty() ? Optional.empty() : Optional.of(jobs.get(0));
	}

	@Override
	public List<UUID> findJobId(final BuildingConsumer<MiningJobInfoInquiryBuilder> builder) {
		return miningJobInfoDao.findJobId(builder);
	}

	@Override
	public void create(final MiningJobInfoPojoPrototype jobInfo) {
		miningJobInfoDao.create(jobInfo);
	}

	@Override
	public int delete(final BuildingConsumer<MiningJobInfoInquiryBuilder> builder) {
		return miningJobInfoDao.delete(builder);
	}

	@Override
	public int deleteByUserName(final String username) {
		final List<UUID> jobIds = jobInfoDao.findIds(q -> q.withCreatedByUserId(username));
		return jobIds.isEmpty() ? 0 : miningJobInfoDao.delete(q -> q.byJobIds(jobIds));
	}
}
