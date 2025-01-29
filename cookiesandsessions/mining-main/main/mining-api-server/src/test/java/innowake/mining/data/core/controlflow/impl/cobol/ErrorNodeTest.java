/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests programs with errors where control flow cannot be determined
 */
public class ErrorNodeTest extends CobolCalculateControlFlowTest {
	
	/**
	 * Tests a program with an empty ELSE block and a PERFOM statement with an undefined target label.
	 */
	@Test
	void testErrors1() {
		final List<String> errorMsgs = Arrays.asList("No statements found in THEN/ELSE block.", "Target could not be resolved.");
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			doTest("error", "ERROR1.cbl");
		});
		final List<String> expectedErrorMsgs = Arrays.asList(exception.getMessage().split(" / "));
		assertThat(errorMsgs, Matchers.containsInAnyOrder(expectedErrorMsgs.toArray()));
	}
	
	/**
	 * Tests a program with an empty THEN block and a GO TO statement with an undefined target label.
	 */
	@Test
	void testErrors2() {
		final List<String> errorMsgs = Arrays.asList("No statements found in THEN/ELSE block.", "Target could not be resolved.");
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			doTest("error", "ERROR2.cbl");
		});
		final List<String> expectedErrorMsgs = Arrays.asList(exception.getMessage().split(" / "));
		assertThat(errorMsgs, Matchers.containsInAnyOrder(expectedErrorMsgs.toArray()));
	}
	
}
