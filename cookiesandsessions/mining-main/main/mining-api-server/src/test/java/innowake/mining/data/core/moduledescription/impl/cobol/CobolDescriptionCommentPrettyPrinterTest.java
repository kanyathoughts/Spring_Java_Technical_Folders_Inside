/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.cobol;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.mining.data.core.moduledescription.api.CommentPrettyPrinter;
import innowake.mining.data.core.moduledescription.api.PrettyPrinter;

/**
 * Unit test for the Cobol description comment pretty printer.
 */
public class CobolDescriptionCommentPrettyPrinterTest extends TestWithResource {

	/**
	 * Test the description comment pretty print with resource PrettyPrinterCase01
	 */
	@Test
	public void testPrettyPrint() {
		final PrettyPrinter printer = CommentPrettyPrinter.INSTANCE;
		final String[] lines = getResource("PrettyPrinterCase01.cbl").split("\n");
		
		final String description = printer.print(Arrays.stream(lines));

		Assert.assertEquals(getResource("PrettyPrinterCase01.exp"), description);
	}
}
