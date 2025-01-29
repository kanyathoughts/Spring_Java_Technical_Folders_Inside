/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server;

import java.io.Serializable;
import java.util.concurrent.TimeUnit;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;

/**
 * A dummy job that runs for maximum of 100 milliseconds with no outcome.
 * Equal to {@link LongDummyTestJob}, but faster.
 */
public class DummyTestJob extends MiningJob<Serializable> {

	public DummyTestJob(final EntityId projectId, final EntityId moduleId) {
		super(projectId, moduleId);
	}
	
	public DummyTestJob(final EntityId projectId) {
		super(projectId);
	}
	
	public DummyTestJob() {
		super(EntityId.VOID);
	}
	
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		for (int i = 0; i < 10; i++) {
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
