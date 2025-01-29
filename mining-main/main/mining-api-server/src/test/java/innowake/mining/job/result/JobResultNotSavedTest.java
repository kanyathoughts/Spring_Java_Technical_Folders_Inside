/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.result;

import static org.mockito.Mockito.doThrow;

import org.junit.jupiter.api.BeforeEach;
import org.mockito.ArgumentMatchers;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.jdbc.datasource.init.UncategorizedScriptException;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.JobInfoService;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Test for job {@link JobResultTestJob} testing the situation where saving the result to the DB fails.
 */
public class JobResultNotSavedTest extends AbstractJobResultTest {

	@SpyBean
	@Nullable
	JobInfoService jobInfoService;

	public JobResultNotSavedTest() {
		super(JobStatus.FAILURE);
	}

	@BeforeEach
	public void initJobLogErrorTest() {
		doThrow(new UncategorizedScriptException("Mocked job result saving error")).when(jobInfoService).createJobResult(ArgumentMatchers.any(), ArgumentMatchers.any());
	}

}
