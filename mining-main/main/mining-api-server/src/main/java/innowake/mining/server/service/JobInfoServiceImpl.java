/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.job.api.JobInfoService;
import innowake.mining.data.access.postgres.JobInfoPgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.entities.JobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.job.Message;

/**
 * Access to {@code job_info} related data.
 */
@Service
public class JobInfoServiceImpl implements JobInfoService {

	private final JobInfoPgDao jobInfoDao;

	@Autowired
	public JobInfoServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		this.jobInfoDao = new JobInfoPgDao(jdbcTemplate);
	}

	@Override
	public UUID upsert(final JobInfoPojoPrototype jobInfo) {
		return jobInfoDao.put(jobInfo, true);
	}

	@Override
	public UUID update(final JobInfoPojoPrototype jobInfo) {
		return jobInfoDao.put(jobInfo, false);
	}

	@Override
	public int createJobResult(final UUID id, final byte[] result) {
		return jobInfoDao.createJobResult(id, result);
	}

	@Override
	public int createJobMessages(final UUID id, final List<Message> messages) {
		return jobInfoDao.createJobMessages(id, messages);
	}

	@Override
	public int addJobMessages(final UUID id, final List<Message> messages) {
		return jobInfoDao.addJobMessages(id, messages);
	}

	@Override
	public List<JobInfoPojo> find(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return jobInfoDao.find(builder);
	}

	@Override
	public Paged<JobInfoPojo> find(final Pagination pagination, final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return jobInfoDao.find(pagination, builder);
	}

	@Override
	public Optional<JobInfoPojo> findAny(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return jobInfoDao.findAny(builder);
	}

	@Override
	public JobInfoPojo get(final UUID id) {
		return findAny(q -> q.byId(id)).orElseThrow(() -> new MiningEntityNotFoundException("JobInfo must exist for id: " + id));
	}

	@Override
	public int delete(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return jobInfoDao.delete(builder);
	}
}
