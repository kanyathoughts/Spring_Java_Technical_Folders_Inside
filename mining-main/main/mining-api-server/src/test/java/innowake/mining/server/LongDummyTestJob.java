/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;

import java.io.Serializable;
import java.util.concurrent.TimeUnit;

/**
 * A dummy job that runs for 10 seconds with no outcome.
 * Equal to {@link DummyTestJob}, but slower.
 */
public class LongDummyTestJob extends MiningJob<Serializable> {

	public LongDummyTestJob(final EntityId projectId, final EntityId moduleId) {
		super(projectId, moduleId);
	}

	public LongDummyTestJob(final EntityId projectId) {
		super(projectId);
	}

	public LongDummyTestJob() {
		super(EntityId.VOID);
	}
	
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		for (int i = 0; i < 1000; i++) {
			progressMonitor.checkCanceled();
			try {
				TimeUnit.MILLISECONDS.sleep(10);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
		}

		return new Result<Serializable>(Status.OK);
	}
}
