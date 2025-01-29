/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.junit.Test;
import innowake.mining.data.core.AstToString;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.ndt.core.parsing.ast.Properties;
import innowake.ndt.pl1parser.ast.model.ProcedureReference;
import innowake.ndt.pl1parser.ast.model.attribute.ExportsAttribute;
import innowake.ndt.pl1parser.ast.model.statement.PackageStatement;

/**
 * Pl/I specific tests for storeAst.
 */
public class Pl1StoreAstTest extends AbstractPl1Test {

	/**
	 * Ast Test to verify {@link ProcedureReference} names collected in {@link Properties} for {@link ExportsAttribute} in {@link PackageStatement}
	 */
	@Test
	public void testPropertiesForPackageStatements() {
		doTest("pl1", "wndt3212.pl1");
	}

	private void doTest(final String folderName, final String... moduleNames) {
		final StoreAstPrototype node = storeAst(folderName, moduleNames);
		assertPropertiesFromPackageStatement(node);
		final String output = new AstToString(this::nodeToString).traverse(node).toString();
		assertOutput(folderName, moduleNames[0], output);
	}

	/**
	 * Assert if we get correct properties for PackageStatement.
	 *
	 * @param rootNode root node of type {@link StoreAstNode}
	 */
	private void assertPropertiesFromPackageStatement(final StoreAstPrototype rootNode) {
		final Optional<StoreAstPrototype> firstEntry = new AstPrototypeCollectingTraverser(
				n -> "PackageStatement".equals(n.type.orElse(null))).collect(rootNode).stream().findFirst();
		assertTrue(firstEntry.isPresent());
		final List<?> procedureNames = (List<?>) firstEntry.get().properties.getNonNull().get(PackageStatement.TARGET_PROCEDURES_NAMES);
		assertEquals(Arrays.asList("PROC1", "NESTED_PROC"), procedureNames);
	}

}
