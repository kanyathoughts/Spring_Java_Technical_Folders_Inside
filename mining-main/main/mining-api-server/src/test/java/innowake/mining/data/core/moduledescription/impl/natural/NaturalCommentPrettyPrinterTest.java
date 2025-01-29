/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.mining.data.core.moduledescription.api.PrettyPrinter;

/**
 * Tests for the Natural comment pretty printer.
 */
public class NaturalCommentPrettyPrinterTest extends TestWithResource {

	@Test
	public void testPrettyPrint() {
		final PrettyPrinter printer = NaturalCommentPrettyPrinter.INSTANCE;
		final String[] lines = getResource("WMIN1170C.nsp").split("\n");
		final String description = printer.print(Arrays.stream(lines));
		Assert.assertEquals(getResource("WMIN1170C.nsp.exp"), description);
	}
}
