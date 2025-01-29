/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.message;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.junit.jupiter.api.Test;

import innowake.mining.job.AbstractJobTest;
import innowake.lib.job.api.management.JobInformation;
import innowake.mining.shared.model.job.Message;

/**
 * Tests for job and task message handling.
 */
public abstract class AbstractJobMessageTest extends AbstractJobTest {

	/**
	 * Tests writing messages from a job.
	 */
	@Test
	public void testMessageFromJob() {
		final MessageJob job = new MessageJob();
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		final List<Message> messages = jobInfo.getMessages();
		assertNotNull(messages);
		assertEquals(3, messages.size());
		assertContains(messages, Message.Severity.INFO, "Info message");
		assertContains(messages, Message.Severity.WARNING, "Warning message");
		assertContains(messages, Message.Severity.ERROR, "Error message");
	}
	
	/**
	 * Tests writing messages from a job and its tasks.
	 */
	@Test
	public void testMessageFromJobWithTasks() {
		final MessageJobWithTasks job = new MessageJobWithTasks();
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);
		
		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		final List<Message> messages = jobInfo.getMessages();
		assertNotNull(messages);
		assertEquals(6, messages.size());
		assertContains(messages, Message.Severity.INFO, "Task 1");
		assertContains(messages, Message.Severity.INFO, "Info message from task");
		assertContains(messages, Message.Severity.INFO, "Task 2");
		assertContains(messages, Message.Severity.WARNING, "Warning message from task");
		assertContains(messages, Message.Severity.INFO, "Task 3");
		assertContains(messages, Message.Severity.ERROR, "Error message from task");
	}
	
	private void assertContains(final List<Message> messages, final Message.Severity severity, final String expectedMessageText) {
		assertTrue(messages.stream().anyMatch(actualMessage -> severity == actualMessage.getSeverity() && expectedMessageText.equals(actualMessage.getText())));
	}

}
