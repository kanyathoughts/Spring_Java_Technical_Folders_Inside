/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol;

import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Technology.ORACLE;
import static innowake.mining.shared.model.Type.CDO_FILE;
import static innowake.mining.shared.model.Type.COPYBOOK;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.discovery.metrics.SourceObjectManager;
import innowake.mining.server.integration.AbstractSourceObjectManagerTest;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link CobolSourceObjectManager}.
 */
class CobolSourceObjectManagerTest extends AbstractSourceObjectManagerTest {

	private static final String MAIN_FOLDER = "cobol";

	private static final String CBL = "cbl";
	private static final String CPY = "cpy";
	private static final String CDO = "cdo";

	@Nullable
	private CobolSourceObjectManager objectManager;

	private final GenericConfigProperties configProperties = new GenericConfigProperties(0, 0, 0, 0, null, 0, 0, 0, 0, 0L, true, false, 8, 5_000_000, -1L,
		10D, 100, null, 100, false, 0, 0, 0, 0, null, 0, 0, 0, 0, 0);

	@BeforeEach
	public void setupObjectManager() {
		objectManager = new CobolSourceObjectManager(Assert.assertNotNull(objectResolver), configProperties);
		assertNotNull(objectManager);
	}

	@Override
	protected SourceObjectManager<?> getObjectManager() {
		return Assert.assertNotNull(objectManager);
	}

	@Test
	void copycodeTest() {
		final String folder = "COPY";
		final SourcePojo prg1 = createSourceObject(folder, "MMRS7101", CBL, PROGRAM);
		final SourcePojo cc1 = createSourceObject(folder, "MMRS710A", CPY, COPYBOOK);
		createSourceObject(folder, "MMRS710B", CPY, COPYBOOK);

		assertSingleDependency(prg1, "MMRS710A", COPYBOOK, true);
		assertSingleDependency(cc1, "MMRS710B", COPYBOOK, true);
	}

	@Test
	void execSqlIncludeTest() {
		final String folder = "EXSQLINC";
		final SourcePojo prg1 = createSourceObject(folder, "MGOASSM1", CBL, PROGRAM);
		createSourceObject(folder, "SMALLSQL", CDO, CDO_FILE, ORACLE);

		final List<?> dependencies = getObjectManager().getOutgoingDependencies(prg1);
		assertDependenciesSize(2, dependencies);
		assertDependency(dependencies, 0, "SQLCA", COPYBOOK, false);
		assertDependency(dependencies, 1, "SMALLSQL", COPYBOOK, false);
	}

	@Test
	void copyFromDictionaryTest() {
		final String folder = "FROMDICT";
		final SourcePojo prg1 = createSourceObject(folder, "WDIS256B", CBL, PROGRAM);
		createSourceObject(folder, "WDIS256A", CDO, CDO_FILE, ORACLE);

		final List<?> dependencies = getObjectManager().getOutgoingDependencies(prg1);
		assertDependenciesSize(1, dependencies);
		assertDependency(dependencies, 0, "CDD_REC.ACE_BENDATA", COPYBOOK, false);
		assertTrue(dependencies.get(0) instanceof CobolDependency);
		assertTrue(((CobolDependency) dependencies.get(0)).isFromDictionary());
	}

	private SourcePojo createSourceObject(final String folder, final String name, final String extension, final Type type) {
		return createSourceObject(MAIN_FOLDER, folder, name, extension, type, COBOL);
	}

	private SourcePojo createSourceObject(final String folder, final String name, final String extension, final Type type, final Technology technology) {
		return createSourceObject(MAIN_FOLDER, folder, name, extension, type, technology);
	}
}
