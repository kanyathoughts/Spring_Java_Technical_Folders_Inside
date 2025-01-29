/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.mocks;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Service;

import innowake.lib.job.api.JobInfoService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.entities.JobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.job.Message;

/**
 * Dummy implementation of the {@link JobInfoService} for testing without a database.
 */
@Service
public class DummyJobInfoService implements JobInfoService {

	@Override
	public UUID upsert(JobInfoPojoPrototype jobInfo) {
		throw new IllegalStateException();
	}

	@Override
	public UUID update(JobInfoPojoPrototype jobInfo) {
		throw new IllegalStateException();
	}

	@Override
	public int createJobResult(UUID id, byte[] result) {
		throw new IllegalStateException();
	}

	@Override
	public int createJobMessages(UUID id, List<Message> messages) {
		throw new IllegalStateException();
	}

	@Override
	public int addJobMessages(UUID id, List<Message> messages) {
		throw new IllegalStateException();
	}

	@Override
	public List<JobInfoPojo> find(BuildingConsumer<JobInfoInquiryBuilder> builder) {
		throw new IllegalStateException();
	}

	@Override
	public Paged<JobInfoPojo> find(Pagination paging, BuildingConsumer<JobInfoInquiryBuilder> builder) {
		throw new IllegalStateException();
	}

	@Override
	public Optional<JobInfoPojo> findAny(BuildingConsumer<JobInfoInquiryBuilder> builder) {
		throw new IllegalStateException();
	}

	@Override
	public JobInfoPojo get(UUID id) {
		throw new IllegalStateException();
	}

	@Override
	public int delete(BuildingConsumer<JobInfoInquiryBuilder> builder) {
		throw new IllegalStateException();
	}

}
