/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.apache.commons.io.FilenameUtils.concat;
import static org.apache.commons.io.FilenameUtils.separatorsToUnix;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.function.Predicate;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.AstTestUtil;
import innowake.mining.shared.TestResourceUtil;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Abstract base class for store ast tests.
 */
public abstract class AbstractStoreAstTest {

	protected static final Charset CHARSET = Charset.forName("cp1252");
	protected static final String EXPECTED_FILE_EXTENSION = ".expected";
	protected boolean writeExpectedFile = Boolean.getBoolean("writeExpected");
	
	protected static final Logger LOG = LoggerFactory.getLogger(AbstractStoreAstTest.class.getCanonicalName());
	
	public AstPrototypeConvertingTraverser createAst(final String folderName, final String... moduleNames) {
		final StoreAstPrototype ast = storeAst(folderName, moduleNames);
		LOG.debug(() -> nodeToString(ast));
		final var converter = new AstPrototypeConvertingTraverser();
		converter.convert(ast);
		return converter;
	}
	
	public StoreAstPrototype storeAst(final String folderName, final String... moduleNames) {
		return storeAst(folderName, null, moduleNames);
	}
	
	public abstract StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> processChildren,
			final String... moduleNames);
	
	protected String getContent(final String folderName, final String moduleName) {
		final String folderPath = getResourceFolderPath(folderName);
		final String testFilePath = separatorsToUnix(concat(folderPath, moduleName));
		try {
			return TestResourceUtil.getContent(testFilePath, CHARSET);
		} catch (final IOException e) {
			throw new IllegalStateException(testFilePath, e);
		}
	}
	
	protected Document getDocument(final String unassembledContent) {
		return new Document(unassembledContent);
	}
	
	protected String nodeToString(final StoreAstPrototype node) {
		return AstTestUtil.nodeToString(new StringBuilder(), node).toString();
	}
	
	/**
	 * Compares the actual output with the expected one
	 *
	 * @param folderName base folder of the test
	 * @param fileName base name of the expected file
	 * @param actualOutput the actual output
	 */
	public void assertOutput(final String folderName, final String fileName, final String actualOutput) {
		try {
			final String folderPath = getResourceFolderPath(folderName);
			final String expectedFilePath = separatorsToUnix(concat(folderPath, fileName + EXPECTED_FILE_EXTENSION));
			if (writeExpectedFile) {
				TestResourceUtil.write(expectedFilePath, actualOutput, CHARSET);
			} else {
				final String expected = TestResourceUtil.getContentNormalized(expectedFilePath, CHARSET);
				assertEquals(expected, actualOutput);
			}
		} catch (final IOException e) {
			fail(e.getMessage());
		}
	}

	/**
	 * Creates the resource folder path based on the test class package and {@code folderName}.
	 *
	 * @param folderName base folder of the test
	 * @return the final resource folder
	 */
	protected String getResourceFolderPath(final String folderName) {
		final String basePath = "/" + StringUtils.replace(getClass().getPackage().getName(), ".", "/");
		return separatorsToUnix(concat(basePath, folderName));
	}
	
	/**
	 * @param offset The offset value
	 * @param length The length value
	 * @return A {@link Tuple2} with the {@link Integer} values of the given {@code offset} and {@code length}
	 */
	protected static Tuple2<Integer, Integer> L(final int offset, final int length) {
		return Tuple2.of(Integer.valueOf(offset), Integer.valueOf(length));
	}
	
	public static ModulePojoDummy createTestModuleDescription(final Long moduleId, final String name, final String content) {
		return new ModulePojoDummy().prepare(m -> m
				.setNid(moduleId)
				.setName(name)
				.setPath(FilenameUtils.getBaseName(name))
				.setContent(content));
	}
}
