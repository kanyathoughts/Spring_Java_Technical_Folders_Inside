/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.event;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.server.job.deletion.BackgroundDeletionExecutionCallback;
import innowake.mining.server.util.TracingHelper;

/**
 * Triggers the background deletion job after a {@link MarkedForDeletionEvent} has been fired.
 */
@Service
public class BackgroundDeletionListener {
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private TracingHelper tracingHelper;
	
	@EventListener
	@WithSystemUser
	public void markedForDeletion(@SuppressWarnings("unused") final MarkedForDeletionEvent event) {
		BackgroundDeletionExecutionCallback.submit(jobManager, tracingHelper);
	}
	
}
