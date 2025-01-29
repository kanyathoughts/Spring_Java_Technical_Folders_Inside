/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.jface.viewers.StyledString;
import org.junit.Test;

import innowake.mining.job.plugin.view.JobViewContentProvider.ParentAwareMessage;
import innowake.mining.shared.model.job.Message;

/**
 * Tests for the {@link JobColumnLabelProvider}.
 */
public class JobColumnLabelProviderTest extends AbstractColumnLabelProviderTest {
	
	private final JobColumnLabelProvider labelProvider;

	/**
	 * Constructor.
	 */
	public JobColumnLabelProviderTest() {
		labelProvider = new JobColumnLabelProvider();
	}
	
	/**
	 * For jobs with a description the description will be printed.
	 */
	@Test
	public void testLabelForJobWithDescription() {
		assertLabel(labelProvider, jobInfoRunning, "Identifying candidates for the project with Id '2'");
	}
	
	/**
	 * For jobs without description, the job Id will be printed.
	 */
	@Test
	public void testLabelForJobWithoutDescription() {
		assertLabel(labelProvider, jobInfoScheduled, "Remote job '" + jobInfoScheduled.getJobInfo().getJobId() + "'");
	}
	
	/**
	 * For message nodes their text will be printed.
	 */
	@Test
	public void testLabelForMessage() {
		final List<Message> messages = jobInfoFailure.getJobInfo().getMessages();
		assertFalse(messages.isEmpty());
		final ParentAwareMessage message = new ParentAwareMessage(messages.get(0), jobInfoFailure);
		
		assertNotNull(labelProvider);
		final StyledString styledString = labelProvider.getStyledText(message);
		assertEquals("Some info message", styledString.getString());
	}
}
