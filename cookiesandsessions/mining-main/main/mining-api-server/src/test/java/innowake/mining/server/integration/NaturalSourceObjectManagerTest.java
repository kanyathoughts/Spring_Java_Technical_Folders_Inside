/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import static innowake.mining.shared.model.Technology.NATURAL;
import static innowake.mining.shared.model.Type.COPYCODE;
import static innowake.mining.shared.model.Type.DDM;
import static innowake.mining.shared.model.Type.GDA;
import static innowake.mining.shared.model.Type.HELP;
import static innowake.mining.shared.model.Type.LDA;
import static innowake.mining.shared.model.Type.MAP;
import static innowake.mining.shared.model.Type.PDA;
import static innowake.mining.shared.model.Type.PROGRAM;
import static innowake.mining.shared.model.Type.SUBPROGRAM;
import static innowake.mining.shared.model.Type.SUBROUTINE;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.server.discovery.metrics.SourceObjectDependency;
import innowake.mining.server.discovery.metrics.SourceObjectManager;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectManager;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link NaturalSourceObjectManager}.
 */
class NaturalSourceObjectManagerTest extends AbstractSourceObjectManagerTest {

	private static final String MAIN_FOLDER = "natural";

	private static final String NSP = "nsp";
	private static final String NSM = "nsm";
	private static final String NSN = "nsn";
	private static final String NSS = "nss";
	private static final String NSH = "nsh";
	private static final String NSC = "nsc";
	private static final String NSD = "nsd";
	private static final String NSIWG = "nsiwg";
	private static final String NSIWA = "nsiwa";
	private static final String NSIWL = "nsiwl";

	@Nullable
	private NaturalSourceObjectManager objectManager;

	private final GenericConfigProperties configProperties = new GenericConfigProperties(0, 0, 0, 0, null, 0, 0, 0, 0, 0L, true, false, 8, 5_000_000, -1L,
		10D, 100, null, 100, false, 0, 0, 0, 0, 0, null, 0, 0, 0, 0);
	private static final String jobId = "job1";
	final TimedWorker timedWorker = new TimedWorkerImpl(new NullProgressMonitor(), null, null);
	final ParseResultCacheService parseResultCacheService = new ParseResultCacheService(configProperties);
	@BeforeEach
	public void setupObjectManager() {
		final NaturalParseResultProvider naturalParseResultProvider = new NaturalParseResultProvider(Assert.assertNotNull(objectResolver), Config.getDefaultConfig(),
				timedWorker, jobId, parseResultCacheService);
		objectManager = new NaturalSourceObjectManager(Assert.assertNotNull(objectResolver), configProperties, naturalParseResultProvider);
		assertNotNull(objectManager);
	}

	@Override
	protected SourceObjectManager<?> getObjectManager() {
		return Assert.assertNotNull(objectManager);
	}

	@Test
	void copycodeTest() {
		final String folder = "COPY";
		final SourcePojo prg1 = createSourceObject(folder, "PRG1", NSP, PROGRAM);
		final SourcePojo cc1 = createSourceObject(folder, "CC1", NSC, COPYCODE);
		createSourceObject(folder, "CC2", NSC, COPYCODE);

		assertSingleDependency(prg1, "CC1", COPYCODE, true);
		assertSingleDependency(cc1, "CC2", COPYCODE, true);
	}

	@Test
	void dataareaTest() {
		final String folder = "DATA";
		final SourcePojo prg2 = createSourceObject(folder, "PRG2", NSP, PROGRAM);
		createSourceObject(folder, "GLOBAL1", NSIWG, GDA);
		createSourceObject(folder, "PARA1", NSIWA, PDA);
		createSourceObject(folder, "LOCAL1", NSIWL, LDA);

		final List<?> dependencies = getObjectManager().getOutgoingDependencies(prg2);
		assertDependenciesSize(3, dependencies);
		assertDependency(dependencies, 0, "GLOBAL1", GDA, true);
		assertDependency(dependencies, 1, "PARA1", PDA, true);
		assertDependency(dependencies, 2, "LOCAL1", LDA, true);

		final SourceObjectDependency lda = (SourceObjectDependency) dependencies.get(2);
		assertNull(lda.getTargetType());
	}

	@Test
	void ddmTest() {
		final String folder = "DDM";
		final SourcePojo prg3 = createSourceObject(folder, "PRG3", NSP, PROGRAM);
		createSourceObject(folder, "DDM1", NSD, DDM);

		assertSingleDependency(prg3, "DDM1", DDM, true);
	}

	@Test
	void logicalNameTest() {
		final String folder = "LOGICAL";
		final SourcePojo prg4 = createSourceObject(folder, "PRG4", NSP, PROGRAM);
		createSourceObject(folder, "SUB1L", NSN, SUBPROGRAM);
		createSourceObject(folder, "SUB2L", NSN, SUBPROGRAM);

		final List<?> dependencies = getObjectManager().getOutgoingDependencies(prg4);
		assertDependenciesSize(2, dependencies);
		assertDependency(dependencies, 0, "SUB1", SUBPROGRAM, false);
		assertDependency(dependencies, 1, "SUB2L", SUBPROGRAM, true);
	}

	@Test
	void subroutineLongNameTest() {
		final String folder = "LONG";
		final SourcePojo prg5 = createSourceObject(folder, "PRG5", NSP, PROGRAM);
		createSourceObject(folder, "SUBR1", NSS, SUBROUTINE);
		createSourceObject(folder, "SUBR2L", NSS, SUBROUTINE);

		final List<?> dependencies = getObjectManager().getOutgoingDependencies(prg5);
		assertDependenciesSize(2, dependencies);
		assertDependency(dependencies, 0, "SUBR1L", SUBROUTINE, false);
		assertDependency(dependencies, 1, "SUBR2L", SUBROUTINE, true);
	}

	@Test
	void mapTest() {
		final String folder = "MAP";
		final SourcePojo prg6 = createSourceObject(folder, "PRG6", NSP, PROGRAM);
		final SourcePojo map1 = createSourceObject(folder, "MAP1", NSM, MAP);
		createSourceObject(folder, "HELP1", NSH, HELP);

		assertSingleDependency(prg6, "MAP1", MAP, true);
		assertSingleDependency(map1, "HELP1", HELP, true);
	}

	@Test
	void subprogramTest() {
		final String folder = "SUBP";
		final SourcePojo prg7 = createSourceObject(folder, "PRG7", NSP, PROGRAM);
		createSourceObject(folder, "SUB1", NSN, SUBPROGRAM);

		assertSingleDependency(prg7, "SUB1", SUBPROGRAM, true);
	}

	@Test
	void substitutionReferenceTest() {
		final String folder = "SUBSTREF";
		final SourcePojo prg8 = createSourceObject(folder, "PRG8", NSP, PROGRAM);
		createSourceObject(folder, "CC3", NSC, COPYCODE);
		createSourceObject(folder, "CC4", NSC, COPYCODE);

		final List<?> dependencies = getObjectManager().getOutgoingDependencies(prg8);
		assertDependenciesSize(2, dependencies);
		assertDependency(dependencies, 0, "CC3", COPYCODE, true);
		assertDependency(dependencies, 1, "CC4", COPYCODE, true);
	}

	private SourcePojo createSourceObject(final String folder, final String name, final String extension, final Type type) {
		return createSourceObject(MAIN_FOLDER, folder, name, extension, type, NATURAL);
	}
}
