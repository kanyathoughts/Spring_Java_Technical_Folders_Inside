package innowake.mining.test;

import static innowake.mining.shared.model.WorkingState.getTargetStates;
import static innowake.mining.shared.model.WorkingState.nextStates;
import static innowake.mining.shared.model.WorkingState.previousState;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import innowake.mining.shared.model.WorkingState;

/**
 * Test for the {@link WorkingState} state transitions.
 */
public class AnnotationStateTest {

	@Test
	public void testNextStates() {
		final List<WorkingState> listForCandidate = Arrays.asList(WorkingState.IN_ANALYSIS);
		final List<WorkingState> listForInAnalysis = Arrays.asList(WorkingState.FOR_REVIEW, WorkingState.REJECTED);
		final List<WorkingState> listForReview = Arrays.asList(WorkingState.APPROVED, WorkingState.INVALID);

		assertEquals(Collections.emptyList(), nextStates(WorkingState.INVALID));
		assertEquals(Collections.emptyList(), nextStates(WorkingState.REJECTED));
		assertEquals(Collections.emptyList(), nextStates(WorkingState.APPROVED));

		assertEquals(listForCandidate, nextStates(WorkingState.CANDIDATE));
		assertEquals(listForInAnalysis, nextStates(WorkingState.IN_ANALYSIS));
		assertEquals(listForReview, nextStates(WorkingState.FOR_REVIEW));

	}

	@Test
	public void testPreviousState() {

		assertFalse(previousState(WorkingState.CANDIDATE).isPresent());

		assertTrue(previousState(WorkingState.APPROVED).isPresent());
		assertEquals(WorkingState.FOR_REVIEW, previousState(WorkingState.APPROVED).get());

		assertTrue(previousState(WorkingState.FOR_REVIEW).isPresent());
		assertEquals(WorkingState.IN_ANALYSIS, previousState(WorkingState.FOR_REVIEW).get());

		assertTrue(previousState(WorkingState.IN_ANALYSIS).isPresent());
		assertEquals(WorkingState.CANDIDATE, previousState(WorkingState.IN_ANALYSIS).get());

		assertTrue(previousState(WorkingState.INVALID).isPresent());
		assertEquals(WorkingState.FOR_REVIEW, previousState(WorkingState.INVALID).get());

		assertTrue(previousState(WorkingState.REJECTED).isPresent());
		assertEquals(WorkingState.IN_ANALYSIS, previousState(WorkingState.REJECTED).get());
	}

	@Test
	public void testGetTargetStates() {

		final List<WorkingState> listForCandidate = Arrays.asList(WorkingState.IN_ANALYSIS);
		final List<WorkingState> listForInAnalysis = Arrays.asList(WorkingState.FOR_REVIEW, WorkingState.REJECTED, WorkingState.CANDIDATE);
		final List<WorkingState> listForReview = Arrays.asList(WorkingState.APPROVED, WorkingState.INVALID, WorkingState.IN_ANALYSIS);
		final List<WorkingState> listForApproved = Arrays.asList(WorkingState.FOR_REVIEW);
		final List<WorkingState> listForRejected = Arrays.asList(WorkingState.IN_ANALYSIS);
		final List<WorkingState> listForInvalid = Arrays.asList(WorkingState.FOR_REVIEW);

		assertEquals(listForCandidate, getTargetStates(WorkingState.CANDIDATE));
		assertEquals(listForInAnalysis, getTargetStates(WorkingState.IN_ANALYSIS));
		assertEquals(listForReview, getTargetStates(WorkingState.FOR_REVIEW));
		assertEquals(listForApproved, getTargetStates(WorkingState.APPROVED));
		assertEquals(listForRejected, getTargetStates(WorkingState.REJECTED));
		assertEquals(listForInvalid, getTargetStates(WorkingState.INVALID));
	}
}
