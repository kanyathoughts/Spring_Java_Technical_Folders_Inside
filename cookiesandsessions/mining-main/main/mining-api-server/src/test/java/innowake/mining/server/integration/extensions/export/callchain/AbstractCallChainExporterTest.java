/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.extensions.export.callchain;

import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.FILE_ACCESS_TYPE;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_CALL_TYPE;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_DATA_ACCESS_BASED;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_DEPTH;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_DIRECTIONS;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_EDGE_PROPERTY_FILTER;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_END_MODULE_IDS;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_END_MODULE_TYPES;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_FILTERED_NAME;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_FILTERED_TYPE;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_IGNORED_BY_TAXONOMY;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_START_MODULE_IDS;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_START_MODULE_TYPES;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Phaser;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.provider.Arguments;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.NonNull;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.export.callchain.CallChainExporter;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * abstract test class for {@link CallChainExporter} tests.
 */
@TestInstance(Lifecycle.PER_CLASS) /* required for access to testData */
@WithMockUser
abstract class AbstractCallChainExporterTest extends DatabaseRelatedTest {
	
	/* empty project */
	static final EntityId TEST_PROJECT_ID = EntityId.of(Long.valueOf(4));
	protected static final String START_MODULE_IDS = PARAMETER_START_MODULE_IDS;
	protected static final String FILTERED_TYPE = PARAMETER_FILTERED_TYPE;
	protected static final String DIRECTIONS = PARAMETER_DIRECTIONS;
	protected static final String FILTERED_NAME = PARAMETER_FILTERED_NAME;
	protected static final String END_MODULE_IDS = PARAMETER_END_MODULE_IDS;
	protected static final String EDGE_PROPERTY_FILTER = PARAMETER_EDGE_PROPERTY_FILTER;
	protected static final String IGNORED_BY_TAXONOMY = PARAMETER_IGNORED_BY_TAXONOMY;
	protected static final String DATA_ACCESS_BASED = PARAMETER_DATA_ACCESS_BASED;
	protected static final String CALL_TYPE = PARAMETER_CALL_TYPE;
	protected static final String DEPTH = PARAMETER_DEPTH;
	protected static final String START_MODULE_TYPES = PARAMETER_START_MODULE_TYPES;
	protected static final String END_MODULE_TYPES = PARAMETER_END_MODULE_TYPES;

	protected static final boolean WRITE_EXPECTED = false;

	/* the expected call types when none are specified */
	static final String DEFAULT_CALL_TYPES = "CALLS, INCLUDES, CONTAINS, REFERENCES, ACCESSES";

	static class TestData {

		final Map<String, ModulePojo> modules = new HashMap<>();

		ModulePojo add(final String name, final ModulePojo module) {
			assertNull(modules.put(name, module), "Module must not be present: " + module.getName());
			return module;
		}

		ModulePojo add(final ModulePojo module) {
			assertNull(modules.put(module.getName(), module), "Module must not be present: " + module.getName());
			return module;
		}

		ModulePojo get(final String name) {
			final ModulePojo module = modules.get(name);
			assertNotNull(module, "Module must be present: " + name);
			return module;
		}
	}

	TestData testData;

	@Autowired
	ModuleService moduleService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	@Autowired
	private JobConfigurationProperties jobConfig;

	@Autowired
	TaxonomyService taxonomyService;

	@Autowired
	private CallChainExporter callChainExporter;

	private UUID type;
	protected EntityId taxonomyA;
	protected EntityId taxonomyB;
	protected EntityId taxonomyC;

	@BeforeAll
	void insertTestData() {
		testData = new TestData();

		/* Constructs the following module graph:
		 *
		 * * ROOTJOB+-+-->ROOTJOB.STEPA+---->PROGRAMA+--+->FILEA
		 *            |                                 |
		 *            +-->ROOTJOB.STEPB+---->PROGRAMB+-++
		 *                                             |
		 *                                             +-->PROGRAMC
		 *
		 *	PROGRAMD+--->PROGRAME+--->COPY1+-+-->PROGRAMF+--->COPY2
		 *                                   |
		 *                                   +-->COPY3
		 *
		 *	PROGRAMG+-+-->TABLEA
		 *            |
		 *            +-->FILEB
		 *
		 *  JOBP------+-->FILEP
		 *            |
		 *            +-->FILEQ<--+--PROGRAMQ--+-->TABLER
		 */
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(TEST_PROJECT_ID));
		type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(TEST_PROJECT_ID).setCategoryId(taxonomyCategory));

		final ModulePojo rootJob = testData.add(createModule("ROOTJOB", Technology.JCL, Type.JOB, Storage.FILE));
		final ModulePojo stepA = testData.add(createModule("ROOTJOB.STEPA", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo stepB = testData.add(createModule("ROOTJOB.STEPB", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo programA = testData.add(createModule("PROGRAMA", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo programB = testData.add(createModule("PROGRAMB", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo programC = testData.add(createModule("PROGRAMC", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo programD = testData.add(createModule("PROGRAMD", Technology.C, Type.PROGRAM, Storage.FILE));
		final ModulePojo programE = testData.add(createModule("PROGRAME", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo programF = testData.add(createModule("PROGRAMF", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.MISSING));
		final ModulePojo programG = testData.add(createModule("PROGRAMG", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo tableA = testData.add(createModule("TABLEA", Technology.SQL, Type.TABLE, Storage.DATABASE));
		final ModulePojo fileA = testData.add(createModule("FILEA", Technology.RESOURCE, Type.FILE, Storage.FILE));
		final ModulePojo fileB = testData.add(createModule("FILEB", Technology.RESOURCE, Type.FILE, Storage.FILE));
		final ModulePojo copy1 = testData.add(createModule("COPY1", Technology.COBOL, Type.COPYBOOK, Storage.FILE));
		final ModulePojo copy2 = testData.add(createModule("COPY2", Technology.COBOL, Type.COPYBOOK, Storage.FILE));
		final ModulePojo copy3 = testData.add(createModule("COPY3", Technology.COBOL, Type.COPYBOOK, Storage.FILE));

		final ModulePojo fileP = testData.add(createModule("FILEP", Technology.RESOURCE, Type.FILE, Storage.FILE));
		final ModulePojo jobP = testData.add(createModule("JOBP", Technology.JCL, Type.JOB, Storage.FILE));
		final ModulePojo fileQ = testData.add(createModule("FILEQ", Technology.RESOURCE, Type.FILE, Storage.FILE));
		final ModulePojo programQ = testData.add(createModule("PROGRAMQ", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo tableR = testData.add(createModule("TABLER", Technology.SQL, Type.TABLE, Storage.DATABASE));

		createReference(RelationshipType.CALLS, rootJob.identity(), stepA.identity());
		createReference(RelationshipType.CALLS, rootJob.identity(), stepB.identity());
		createReference(RelationshipType.CALLS, stepA.identity(), programA.identity());
		createReference(RelationshipType.ACCESSES, programA.identity(), fileA.identity());
		createReference(RelationshipType.CALLS, stepB.identity(), programB.identity());
		createReference(RelationshipType.ACCESSES, programB.identity(), fileA.identity());
		createReference(RelationshipType.CALLS, programB.identity(), programC.identity());
		createReference(RelationshipType.CALLS, programD.identity(), programE.identity());
		createReference(RelationshipType.INCLUDES, programE.identity(), copy1.identity());
		createReference(RelationshipType.INCLUDES, programF.identity(), copy2.identity());
		createReference(RelationshipType.INCLUDES, copy1.identity(), copy3.identity());
		createReference(RelationshipType.CALLS, copy1.identity(), programF.identity());

		createReferenceWithProperties(RelationshipType.ACCESSES, programG.identity(), tableA.identity(), DB_ACCESS_TYPE.name(), "SELECT");
		createReferenceWithProperties(RelationshipType.ACCESSES, programG.identity(), fileB.identity(), FILE_ACCESS_TYPE.name(), "WRITE,READ");

		taxonomyA = createTaxonomy(type, "Taxonomy A");
		taxonomyB = createTaxonomy(type, "Taxonomy B");
		taxonomyC = createTaxonomy(type, "Taxonomy C");
		assignTaxonomy(programA.getUid(), taxonomyA);
		assignTaxonomy(programB.getUid(), taxonomyB);
		assignTaxonomy(stepA.getUid(), taxonomyC);
		assignTaxonomy(programD.getUid(), taxonomyA);

		createReferenceWithProperties(RelationshipType.ACCESSES, jobP.identity(), fileP.identity(), FILE_ACCESS_TYPE.name(), "READ");
		createReferenceWithProperties(RelationshipType.ACCESSES, jobP.identity(), fileQ.identity(), FILE_ACCESS_TYPE.name(), "WRITE");
		createReferenceWithProperties(RelationshipType.ACCESSES, programQ.identity(), fileQ.identity(), FILE_ACCESS_TYPE.name(), "READ");
		createReferenceWithProperties(RelationshipType.ACCESSES, programQ.identity(), tableR.identity(), DB_ACCESS_TYPE.name(), "UPDATE");

		/*                 +----------------------------------------------- <----+
		 *                 |                                                     |
		 *                 v                                                     |
		 *  PROGA1--+--> PROGA2---> PROGA3---> PROGA4---> PROGA5-------------> --+
		 *          |                                                            |
		 *          +--> PROGB1---> PROGA3---> PROGA4---> PROGA5-------------> --+
		 *          |                                                            |
		 *          +--> PROGB2--> PROGB1---> PROGA3---> PROGA4---> PROGA5---> --+
		 */
		final ModulePojo progA1 = testData.add(createModule("progA1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progA2 = testData.add(createModule("progA2", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progA3 = testData.add(createModule("progA3", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progA4 = testData.add(createModule("progA4", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progA5 = testData.add(createModule("progA5", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progB1 = testData.add(createModule("progB1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progB2 = testData.add(createModule("progB2", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		createReference(RelationshipType.CALLS, progA1.identity(), progA2.identity());
		createReference(RelationshipType.CALLS, progA1.identity(), progB1.identity());
		createReference(RelationshipType.CALLS, progA1.identity(), progB2.identity());
		createReference(RelationshipType.CALLS, progA2.identity(), progA3.identity());
		createReference(RelationshipType.CALLS, progA3.identity(), progA4.identity());
		createReference(RelationshipType.CALLS, progA4.identity(), progA5.identity());
		createReference(RelationshipType.CALLS, progB1.identity(), progA3.identity());
		createReference(RelationshipType.CALLS, progB2.identity(), progB1.identity());
		createReference(RelationshipType.CALLS, progA5.identity(), progA2.identity());

		/*   +---------------+
		 *   |               |
		 *   v               |
		 * PROGL1--->PROGL2--+
		 */
		final ModulePojo progL1 = testData.add(createModule("progL1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progL2 = testData.add(createModule("progL2", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		createReference(RelationshipType.CALLS, progL1.identity(), progL2.identity());
		createReference(RelationshipType.CALLS, progL2.identity(), progL1.identity());

		/* PROGC1----> PROGC2-+----READ,IPSUM---+----> PROGC3
		 *                    |                 |
		 *                    +----READ---------+
		 *                    |                 |
		 *                    +----WRITE--------+
		 *                    |                 |
		 *                    +----WRITE,READ---+
		 *                    |                 |
		 *                    +-----------------+
		 */
		final ModulePojo progC1 = testData.add(createModule("progC1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progC2 = testData.add(createModule("progC2", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progC3 = testData.add(createModule("progC3", Technology.COBOL, Type.PROGRAM, Storage.FILE));

		createReference(RelationshipType.CALLS, progC1.identity(), progC2.identity());
		createReferenceWithProperties(RelationshipType.CALLS, progC2.identity(), progC3.identity(), FILE_ACCESS_TYPE.name(), "READ", "LOREM", "IPSUM");
		createReferenceWithProperties(RelationshipType.CALLS, progC2.identity(), progC3.identity(), FILE_ACCESS_TYPE.name(), "READ");
		createReferenceWithProperties(RelationshipType.CALLS, progC2.identity(), progC3.identity(), FILE_ACCESS_TYPE.name(), "WRITE");
		createReferenceWithProperties(RelationshipType.CALLS, progC2.identity(), progC3.identity(), FILE_ACCESS_TYPE.name(), "WRITE,READ");
		createReference(RelationshipType.CALLS, progC2.identity(), progC3.identity());

		/*  PROGD1--+--> PROGD2---> PROGD3---> PROGD4
		 *          |
		 *          +--> PROGD5---> PROGD6---> PROGD7
		 *          |
		 *          +--> PROGD8
		 *
		 * PROGD1---> PROGD2: CALL_TYPE=CALL, FILE_ACCESS_TYPE=READ
		 * PROGD2---> PROGD3:
		 * PROGD3---> PROGD4: CALL_TYPE=CALL, LOREM=IPSUM
		 *
		 * PROGD1---> PROGD5: CALL_TYPE=MAP, FILE_ACCESS_TYPE=READ
		 * PROGD5---> PROGD6: LOREM=IPSUM
		 * PROGD6---> PROGD7: CALL_TYPE=MAP, LOREM=IPSUM
		 *
		 * PROGD1---> PROGD8: CALL_TYPE=CALL
		 */
		final ModulePojo progD1 = testData.add(createModule("progD1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD2 = testData.add(createModule("progD2", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD3 = testData.add(createModule("progD3", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD4 = testData.add(createModule("progD4", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD5 = testData.add(createModule("progD5", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD6 = testData.add(createModule("progD6", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD7 = testData.add(createModule("progD7", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo progD8 = testData.add(createModule("progD8", Technology.COBOL, Type.PROGRAM, Storage.FILE));

		createReferenceWithProperties(RelationshipType.CALLS, progD1.identity(), progD2.identity(), "CALL_TYPE", "CALL", FILE_ACCESS_TYPE.name(), "READ");
		createReference(RelationshipType.CALLS, progD2.identity(), progD3.identity());
		createReferenceWithProperties(RelationshipType.CALLS, progD3.identity(), progD4.identity(), "CALL_TYPE", "CALL", "LOREM", "IPSUM");

		createReferenceWithProperties(RelationshipType.CALLS, progD1.identity(), progD5.identity(), "CALL_TYPE", "MAP", FILE_ACCESS_TYPE.name(), "READ");
		createReferenceWithProperties(RelationshipType.CALLS, progD5.identity(), progD6.identity(), "LOREM", "IPSUM");
		createReferenceWithProperties(RelationshipType.CALLS, progD6.identity(), progD7.identity(), "CALL_TYPE", "MAP", "LOREM", "IPSUM");

		createReferenceWithProperties(RelationshipType.CALLS, progD1.identity(), progD8.identity(), "CALL_TYPE", "CALL");

		/*   +----------------------------+
		 *   |                            |
		 *   v                            |
		 * MMRS71B1--+--> MMRS71C1------->+
		 *           |                    |
		 * 	         +------------------->+
		 *                                |
		 *                                v
		 * MMRS710J--+--> MMRS7101---> MMRS71Z1
		 */
		final ModulePojo MMRS710J = testData.add(createModule("MMRS710J", Technology.JCL, Type.JOB, Storage.FILE));
		final ModulePojo MMRS71Z1 = testData.add(createModule("MMRS71Z1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo MMRS7101 = testData.add(createModule("MMRS7101", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo MMRS71B1 = testData.add(createModule("MMRS71B1", Technology.COBOL, Type.PROGRAM, Storage.FILE));
		final ModulePojo MMRS71C1 = testData.add(createModule("MMRS71C1", Technology.COBOL, Type.PROGRAM, Storage.FILE));

		createReference(RelationshipType.CALLS, MMRS710J.identity(), MMRS7101.identity());
		createReference(RelationshipType.CALLS, MMRS7101.identity(), MMRS71Z1.identity());
		createReference(RelationshipType.CALLS, MMRS71B1.identity(), MMRS71C1.identity());
		createReference(RelationshipType.CALLS, MMRS71C1.identity(), MMRS71Z1.identity());
		createReference(RelationshipType.CALLS, MMRS71B1.identity(), MMRS71Z1.identity());
		createReference(RelationshipType.CALLS, MMRS71C1.identity(), MMRS71B1.identity());

		/*
		 * MMRS712K------>MMRS712K.AWA.STEPCP.EXEC---->MMRS712P--+-->MMRS712P.STEPDELE.EXEC_PGM---------------+
		 *                                                       |                                            |
		 *                                                       |                                            v
		 *                                                       +-->MMRS712P.STEPCOPY.EXEC_PGM-->MMRS00C.AWA.MMRS712K.COPYFILE
		 *                                                                     |
		 *                                                                     v
		 * MMRS712U---+-->MMRS712U.STEPVSKR.EXEC_PGM-----+------>MMRS00C.AWA.VSAMK.UNLOAD
		 *            |                                  |
		 *            +-->MMRS712U.STEPVSK.EXEC_PGM------+
		 *            |                                  |
		 *            +-->MMRS712U.STEPDEL.EXEC_PGM------+
		 */
		final ModulePojo MMRS712K = testData.add(createModule("MMRS712K", Technology.JCL, Type.JOB, Storage.FILE));
		final ModulePojo STEPCP = testData.add(createModule("STEPCP", Technology.JCL, Type.EXEC, Storage.FILE));
		final ModulePojo MMRS712P = testData.add(createModule("MMRS712P", Technology.JCL, Type.PROC, Storage.FILE));
		final ModulePojo STEPDELE = testData.add(createModule("STEPDELE", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo STEPCOPY = testData.add(createModule("STEPCOPY", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo MMRS712U = testData.add(createModule("MMRS712U", Technology.JCL, Type.JOB, Storage.FILE));
		final ModulePojo STEPVSKR = testData.add(createModule("STEPVSKR", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo STEPVSK = testData.add(createModule("STEPVSK", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo STEPDEL = testData.add(createModule("STEPDEL", Technology.JCL, Type.EXEC_PGM, Storage.FILE));
		final ModulePojo COPYFILE = testData.add(createModule("COPYFILE", Technology.RESOURCE, Type.FILE, Storage.FILE));
		final ModulePojo UNLOAD = testData.add(createModule("UNLOAD", Technology.RESOURCE, Type.FILE, Storage.FILE));

		createReference(RelationshipType.CALLS, MMRS712K.identity(), STEPCP.identity());
		createReference(RelationshipType.CALLS, STEPCP.identity(), MMRS712P.identity());
		createReference(RelationshipType.CALLS, MMRS712P.identity(), STEPDELE.identity());
		createReference(RelationshipType.CALLS, MMRS712P.identity(), STEPCOPY.identity());
		createReference(RelationshipType.ACCESSES, STEPDELE.identity(), COPYFILE.identity());
		createReference(RelationshipType.ACCESSES, STEPCOPY.identity(), COPYFILE.identity());
		createReference(RelationshipType.ACCESSES, STEPCOPY.identity(), UNLOAD.identity());
		createReference(RelationshipType.CALLS, MMRS712U.identity(), STEPVSKR.identity());
		createReference(RelationshipType.CALLS, MMRS712U.identity(), STEPVSK.identity());
		createReference(RelationshipType.CALLS, MMRS712U.identity(), STEPDEL.identity());
		createReference(RelationshipType.ACCESSES, STEPVSKR.identity(), UNLOAD.identity());
		createReference(RelationshipType.ACCESSES, STEPVSK.identity(), UNLOAD.identity());
		createReference(RelationshipType.ACCESSES, STEPDEL.identity(), UNLOAD.identity());

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype().setProject(programF.getProject()).setModule(EntityId.of(programF.getUid()))
				.setCause("Sample cause").setKey(ErrorKey.EMPTY_FILE).setSeverity(Severity.ERROR));
	}

	@SuppressWarnings("unchecked")
	private Job<FileSystemResult> createCallChainExportJob(final Map<String, List<String>> parameters) {
		parameters.put(Parameters.PARAMETER_COMPRESSED, Collections.singletonList("false"));
		return callChainExporter.createJob(TEST_PROJECT_ID, parameters, mock(HttpEntity.class));
	}

	String callChainJob(final Map<String, List<String>> parameters) {
		final String jobId;
		final Span rootSpan = tracer.newTrace();
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final Phaser jobFinishedPhase = new Phaser(2);
			final Throwable[] error = new Throwable[1];
			jobId = jobManager.submit(createCallChainExportJob(parameters), new JobExecutionCallback() {

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					jobFinishedPhase.arrive();
				}

				@Override
				public void onCompletion() {
					jobFinishedPhase.arrive();
				}
			}).getJobId();
			jobFinishedPhase.arriveAndAwaitAdvance();
			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
		} finally {
			rootSpan.finish();
		}
		final File resultFile = Paths.get(jobConfig.getJobResultFolder(), jobId).toFile();
		assertTrue(resultFile.exists(), () -> "File doesn't exist");
		try {
			return new String(Files.readAllBytes(Paths.get(resultFile.getAbsolutePath())), StandardCharsets.UTF_8);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	ModulePojo createModule(final String name, final Technology technology, final Type type, final Storage storage, final String... path) {
		return createModule(name, technology, type, storage, Identification.IDENTIFIED, path);
	}

	ModulePojo createModule(final String name, final Technology technology, final Type type, final Storage storage, final Identification identification,
			final String... path) {
		return moduleService.getModule(moduleService.create(new ModulePojoPrototype()
				.setProject(TEST_PROJECT_ID)
				.setName(name)
				.setTechnology(technology)
				.setType(type)
				.setPath(path.toString())
				.setStorage(storage)
				.setIdentification(identification)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)));
	}

	void createReference(final RelationshipType relationship, final EntityId fromId, final EntityId toId) {
		createReferenceWithPropertyMap(relationship, fromId, toId, (Map<String, Object>) null, null);
	}
	
	void createReference(final RelationshipType relationship, final EntityId fromId, final EntityId toId, final List<EntityId> conditionalDependency) {
		createReferenceWithPropertyMap(relationship, fromId, toId, (Map<String, Object>) null, conditionalDependency);
	}
	
	void createReferenceWithProperties(final RelationshipType relationship, final EntityId fromId, final EntityId toId, final String... keyValueProperties) {
		assertEquals(0, keyValueProperties.length % 2, "Properties must be pairs of key and value");

		final Map<String, Object> properties = new HashMap<>();
		for (int i = 0; i < keyValueProperties.length; i+= 2) {
			properties.put(keyValueProperties[i], keyValueProperties[i + 1]);
		}

		createReferenceWithPropertyMap(relationship, fromId, toId, properties, null);
	}
	
	void createReferenceWithPropertyMap(final RelationshipType relationship, final EntityId fromId, final EntityId toId,
			@Nullable final Map<String, Object> properties, @Nullable final List<EntityId> conditionalDependency) {
		final var relation = new ModuleRelationshipPojoPrototype()
					.setRelationship(relationship)
					.setSrcModule(fromId)
					.setDstModule(toId);
		
		if (properties != null){
			relation.setProperties(properties);
		}
		
		if (conditionalDependency != null) {
			relation.setValidIfReachedFrom(conditionalDependency);
		}
		
		moduleService.createRelationship(relation);
	}
	
	/**
	 * @return test arguments for parameterized test method {@code #test(String, Map)}
	 */
	public Stream<Arguments> testCases() {
		return Stream.of(
			/* The callchain of ROOTJOB only contains Steps, Programs and Files, so if we filter those
			 * out, we expect an empty graph. */
			arguments("Empty call chain", parameters(START_MODULE_IDS, getModuleId("ROOTJOB"), FILTERED_TYPE, createList("EXEC_PGM", "PROGRAM", "FILE"))),
			
			/* For no parameters, we expect multiple graphs of all the modules followed by its respective callchains. */
			arguments("No start module Ids", parameters()),
			
			/* Creates a graph which starts with ROOTJOB followed by its respective callchains. */
			arguments("One start module Id", parameters(START_MODULE_IDS, getModuleId("ROOTJOB"))),
			
			/* Creates a graph of incoming callchain edges from FILEA. */
			arguments("Direction IN", parameters(START_MODULE_IDS, createList(getModuleId("FILEA")), DIRECTIONS, createList("IN"))),
			
			/* Creates a graph of outgoing callchain edges from progA1 filtering out progA2 and progA5. */
			arguments("Direction OUT",
					parameters(START_MODULE_IDS, getModuleId("progA1"), DIRECTIONS, createList("OUT"), FILTERED_NAME, createList("progA2", "progA5"))),
			
			/* Creates graphs of outgoing and incoming callchain edges for each created modules. */
			arguments("Direction BOTH", parameters(DIRECTIONS, createList("IN", "OUT"))),
			
			/* Creates graphs of callchain edges for multiple start modules. */
			arguments("Multiple start module Ids", parameters(START_MODULE_IDS, createList(getModuleId("PROGRAMD"), getModuleId("PROGRAMA")))),

			/* Creates a graph of callchain edges from ROOTJOB to PROGRAMB. */
			arguments("Filter End Modules", parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), END_MODULE_IDS, createList(getModuleId("PROGRAMB")))),
			
			/* Creates a graph of callchain edges from ROOTJOB, filtering out PROGRAMB from the graph. */
			arguments("Filtered Name", parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), FILTERED_NAME, createList("PROGRAMB"))),

			/* Creates a graph of callchain edges from ROOTJOB, filtering out EXEC_PGM type from the graph. */
			arguments("Filtered Type", parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), FILTERED_TYPE, createList("EXEC_PGM"))),

			/* Creates a graph filtering out the final module from the call chain if it matches the "filteredTypes" and also remove the FILE type from the 
			 * last entry. */
			arguments("Filtered Type EndModule", parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), FILTERED_TYPE, createList("FILE"))),

			/* We expect a graph which should NOT filter the start module from the call chain even if it matches the "filteredTypes". */
			arguments("Filtered Type StartModule", parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), FILTERED_TYPE, createList("JOB"))),

			/* We expect a graph which should NOT filter the start module from the call chain even if it matches the "filteredTypes". */
			arguments("Filtered Type StartModule DirectionIn",
					parameters(START_MODULE_IDS, createList(getModuleId("FILEA")), FILTERED_TYPE, createList("FILE"), DIRECTIONS, createList("IN"))),
			
			/* We expect a graph having outgoing callchain from PROGA1 and should filter looping modules from the call chain if it matches the "filteredName".
			 *                 +----------------------------------------------- <--------+
			 *                 |                                                         |
			 *                 v                                                         |
			 *  PROGA1--+--> ( PROGA2---> ) PROGA3---> PROGA4---> ( PROGA5---------> ) --+
			 *          |                                                                |
			 *          +--> PROGB1---> PROGA3---> PROGA4---> ( PROGA5-------------> ) --+
			 *          |                                                                |
			 *          +--> PROGB2--> PROGB1---> PROGA3---> PROGA4---> ( PROGA5---> ) --+ */
			arguments("Multiple Filtered Names With LoopInCallChain",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progA1")), FILTERED_NAME,
							createList("progA2", "progA5"))),
			
			/* We expect a graph having outgoing callchain from PROGD1 including only edges that contain the property CALL_TYPE with value CALL.
			 * 
			 * PROGD1--+--> PROGD2---> PROGD3---> PROGD4
			 *          |
			 *          +--> PROGD5---> PROGD6---> PROGD7
			 *          |
			 *          +--> PROGD8
			 * 
			 * PROGD1---> PROGD2: CALL_TYPE=CALL, FILE_ACCESS_TYPE=READ
			 * PROGD2---> PROGD3:
			 * PROGD3---> PROGD4: CALL_TYPE=CALL, LOREM=IPSUM
			 * 
			 * PROGD1---> PROGD5: CALL_TYPE=MAP, FILE_ACCESS_TYPE=READ
			 * PROGD5---> PROGD6: LOREM=IPSUM
			 * PROGD6---> PROGD7: CALL_TYPE=MAP, LOREM=IPSUM
			 * 
			 * PROGD1---> PROGD8: CALL_TYPE=CALL
			 */
			arguments("EdgePropertyFilterWithEQ",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progD1")), EDGE_PROPERTY_FILTER,
							createList("{\"CALL_TYPE\": {\"eq\": \"CALL\"}}"))),

			/* We expect a graph having outgoing callchain from PROGD1 including only edges that contain the property CALL_TYPE with value CALL or MAP.*/
			arguments("EdgePropertyFilterWithOrEQ",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progD1")), EDGE_PROPERTY_FILTER,
							createList("{\"_or\": [{\"CALL_TYPE\": {\"eq\": \"CALL\"}},{\"CALL_TYPE\": {\"eq\": \"MAP\"}}]}"))),
			
			/* We expect a graph having outgoing callchain from PROGD1 including only edges that contain the property LOREM with value null.*/
			arguments("EdgePropertyFilterWithEQNull",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progD1")), EDGE_PROPERTY_FILTER,
							createList("{\"LOREM\": {\"eq\": null}}"))),
			
			/* Creates a graph for looping modules and filters module from the call chain if it matches the "filteredName".
			 *   +-------------------+
			 *   |                   |
			 *   v                   |
			 * PROGL1 (--->PROGL2)---+ 
			 */ 
			arguments("Filtered Name With ShortLoop",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progL1")), FILTERED_NAME, createList("progL2"))),
			
			/* Creates a graph of outgoing callchain edges from progA1 and should ignore creating the same call chain twice till depth 2. */
			arguments("Export With Duplicated Edge DirectionOutDepth",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progA1")), DEPTH, createList("2"))),
			
			/* Creates a graph of outgoing callchain edges from progA1 and should ignore creating the same call chain twice 
			 *                 +----------------------------------------------- <----+
			 *                 |                                                     |
			 *                 v                                                     |
			 *  PROGA1--+--> PROGA2---> PROGA3---> PROGA4---> PROGA5-------------> --+
			 *          |                                                            |
			 *          +--> PROGB1---> PROGA3---> PROGA4---> PROGA5-------------> --+
			 *          |                                                            |
		     *          +--> PROGB2--> PROGB1---> PROGA3---> PROGA4---> PROGA5---> --+ 
		     */
			arguments("Export With Duplicated Edge DirectionOut",
					parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS, createList(getModuleId("progA1")))),
			
			/* Creates a graph of outgoing callchain edges from ROOTJOB and ignoring all in's & out's of the module with assigned taxonomy. */
			arguments("SingleIgnoreTaxonomy",
					parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), DIRECTIONS, createList("OUT"), IGNORED_BY_TAXONOMY,
							Arrays.asList(taxonomyA.getNid().toString()))),
			
			/* Tests filtering of modules by taxonomies.
			 * We expect a graph of outgoing callchain edges starting from ROOTJOB. Modules (PROGRAMA, PROGRAMB, PROGRAMD) that are linked with taxonomyA 
			 * or taxonomyB must be filtered and not be present in the graph. 
			 */			
			arguments("MultiIgnoreTaxonomy",
					parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), DIRECTIONS, createList("OUT"), IGNORED_BY_TAXONOMY,
							Arrays.asList(taxonomyA.getNid().toString(), taxonomyB.getNid().toString()))),
			
			/* Creates a graph of outgoing callchain edges from ROOTJOB for all modules as no taxonomy is been ignored. */
			arguments("EmptyIgnoreTaxonomy",
					parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), DIRECTIONS, createList("OUT"), IGNORED_BY_TAXONOMY,
							Collections.emptyList())),
			
			/* Creates a graph of outgoing callchain edges from ROOTJOB, ignoring all in's & out's of the module with assigned taxonomy. */
			arguments("WithMidModuleTaxonomy",
					parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), DIRECTIONS, createList("OUT"), IGNORED_BY_TAXONOMY,
							Arrays.asList(taxonomyC.getNid().toString()))),
			
			/* Creates a graph with no callchain edges, as we ignore all the edges with taxanomy id of taxonomyA. */
			arguments("WithStartModuleTaxonomy",
					parameters(START_MODULE_IDS, createList(getModuleId("PROGRAMD")), DIRECTIONS, createList("OUT"), IGNORED_BY_TAXONOMY,
							Arrays.asList(taxonomyA.getNid().toString()))),
			
			/* Creates a graph of outgoing callchain edges from FILEP based on data access and call type. */
			arguments("OutBoundDataAccessBasedFile",
					parameters(DIRECTIONS, createList("OUT"), DATA_ACCESS_BASED, createList("true"), CALL_TYPE, createList("ACCESSES"), 
							START_MODULE_IDS, createList(getModuleId("FILEP")))),
			
			/* Creates a graph of incoming callchain edges from TABLER based on data access and call type. 
			 * 
			 *  FILEP----> JOBP------> FILEQ-----> PROGRAMQ ------> TABLER
			 */
			arguments("InBoundDataAccessBasedFile", parameters(DIRECTIONS, createList("IN"), DATA_ACCESS_BASED, createList("true"), CALL_TYPE,
					createList("ACCESSES"), START_MODULE_IDS, createList(getModuleId("TABLER")))),
			
			/* Creates a graph of all incoming and callchain edges from MMRS71Z1 to MMRS710J. */
			arguments("Direction BOTH with end Modules", parameters(DIRECTIONS, createList("IN", "OUT"), START_MODULE_IDS,
					createList(getModuleId("MMRS71Z1")), END_MODULE_IDS, createList(getModuleId("MMRS710J")))),
			
			/* Creates graphs of outgoing callchain edges for each created modules. */
			arguments("outboundCallChainWithNoStartModule", parameters(DIRECTIONS, createList("OUT"))),
			
			/* Creates graphs of incoming callchain edges for each created modules. */
			arguments("inboundCallWithNoStartModule", parameters(DIRECTIONS, createList("IN"))),
			
			/* We expect a graph having outgoing callchain from PROGD1 including only edges that contain the property LOREM with value IPSUM.*/
			arguments("edgePropertyFilterWithEQNoMatch", parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS,
					createList(getModuleId("progD1")), EDGE_PROPERTY_FILTER, createList("{\"LOREM\": {\"eq\": \"IPSUM\"}}"))),
			
			/* We expect a graph having outgoing callchain from PROGD1 including only edges that contain the property CALL_TYPE with value CALL
			 * and FILE_ACCESS_TYPE with value READ. */
			arguments("edgePropertyFilterWithAndEQ", parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS,
						createList(getModuleId("progD1")), EDGE_PROPERTY_FILTER,
						createList("{\"_and\": [{\"CALL_TYPE\": {\"eq\": \"CALL\"}},{\"FILE_ACCESS_TYPE\": {\"eq\": \"READ\"}}]}"))),
			
			/* We expect a graph having outgoing callchain from PROGD1 excluding all edges that contain the property CALL_TYPE with value CALL.*/
			arguments("edgePropertyFilterWithNotEQ", parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS,
					createList(getModuleId("progD1")), EDGE_PROPERTY_FILTER, createList("{\"_not\": {\"CALL_TYPE\": {\"eq\": \"CALL\"}}}"))),
			
			/* Creates a graph for outgoing edges from progC1, One of the two READs is filtered and not exported
			 *  PROGC1----> PROGC2-+----READ.IPSUM---+----> PROGC3
			 *                    |                 |
			 *                    +----READ---------+
			 *                    |                 |
			 *                    +----WRITE--------+
			 *                    |                 |
			 *                    +----WRITE,READ---+
			 *                    |                 |
			 *                    +-----------------+
			 */
			arguments("filterDuplicatesByAttributes", parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS,
					createList(getModuleId("progC1")))),
			
			/* We expect a graph having outgoing callchain from PROGA1 and should filter looping modules from the call chain if it matches the "filteredName".
			 *                 +----------------------------------------------- <--------+
			 *                 |                                                         |
			 *                 v                                                         |
			 *  PROGA1--+--> ( PROGA2---> ) PROGA3---> PROGA4---> PROGA5-------------> --+
			 *          |                                                                |
			 *          +--> PROGB1---> PROGA3---> PROGA4---> PROGA5-------------> ------+
			 *          |                                                                |
			 *          +--> PROGB2--> PROGB1---> PROGA3---> PROGA4---> PROGA5---> ------+ */
			arguments("filteredNameWithLoopInCallChain2", parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS,
					createList(getModuleId("progA1")), FILTERED_NAME, createList("progA2"))),
			
			/* We expect a graph having outgoing callchain from PROGA1 and should filter looping modules from the call chain if it matches the "filteredName".
			 *                  +----------------------------------------------- <--------+
			 *                 |                                                         |
			 *                 v                                                         |
			 *  PROGA1--+--> PROGA2---> PROGA3---> PROGA4---> ( PROGA5-------------> ) --+
			 *          |                                                                |
			 *          +--> PROGB1---> PROGA3---> PROGA4---> ( PROGA5-------------> ) --+
			 *          |                                                                |
			 *          +--> PROGB2--> PROGB1---> PROGA3---> PROGA4---> ( PROGA5---> ) --+ */
			arguments("filteredNameWithLoopInCallChain", parameters(DIRECTIONS, createList("OUT"), START_MODULE_IDS,
					createList(getModuleId("progA1")), FILTERED_NAME, createList("progA5"))),
			
			/* Creates a graph of incoming and outgoing edges from ROOTJOB.STEPA. */
			arguments("exportCallChainOrdersStepA", parameters(DIRECTIONS, createList("OUT", "IN"), START_MODULE_IDS,
					createList(getModuleId("ROOTJOB.STEPA")))),
			
			/* Creates a graph of incoming and outgoing edges from PROGRAMB. */
			arguments("exportCallChainOrders", parameters(DIRECTIONS, createList("OUT", "IN"), START_MODULE_IDS,
					createList(getModuleId("PROGRAMB")))),
			
			/* We expect an graph of incoming edges to ROOTJOB.STEPA filtering out the module from the call chain if it matches the "filteredTypes". */
			arguments("exportCallChainIncomingAllFiltered", parameters(DIRECTIONS, createList("IN"), START_MODULE_IDS,
					createList(getModuleId("ROOTJOB.STEPA")), FILTERED_TYPE, createList("JOB"))),
			
			/* We expect an empty graph as the specified endModuleId does not exist. */
			arguments("exportCallChainNoResults",
						parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), END_MODULE_IDS, createList("0"))),
			
			/* We expect a graph having callchain from ROOTJOB till the level of depth of recursion. */
			arguments("exportCallChainDepth", parameters(DEPTH, createList("0"), START_MODULE_IDS, createList(getModuleId("ROOTJOB")))),
			
			/* it should NOT export the same call chain twice */
			arguments("exportCallChainDuplicate", parameters(FILTERED_TYPE, createList("PROGRAM", "FILE"), START_MODULE_IDS,
					createList(getModuleId("ROOTJOB")))),
			
			/*Creates a graph of incoming callchain edges from FILEB based on call type filtering file type module from the call chain. */
			arguments("exportCallChainFileAccessOperation", parameters(FILTERED_TYPE, createList("FILE"), START_MODULE_IDS,
					createList(getModuleId("FILEB")), CALL_TYPE, createList("ACCESSES"), DIRECTIONS, createList("IN"))),
			
			/*Creates a graph of incoming callchain edges from TABLEA based on call type filtering out VIEW type module from the call chain. */
			arguments("exportCallChainDatabaseAccessOperation", parameters(FILTERED_TYPE, createList("VIEW"), START_MODULE_IDS,
					createList(getModuleId("TABLEA")), CALL_TYPE, createList("ACCESSES"), DIRECTIONS, createList("IN"))),
			
			/* Creates a graph of callchain edges from PROGRAMB based on call type. */
			arguments("exportCallChainCallType",
					parameters(START_MODULE_IDS, createList(getModuleId("PROGRAMB")), CALL_TYPE, Collections.singletonList("ACCESSES"))),
			
			/* Creates a graph of callchain edges from PROGRAMB based on call type. */
			arguments("exportCallChainWithOnlyCallType", parameters(CALL_TYPE, Collections.singletonList("Includes"))),
			
			/* Creates a graph of callchain edges from PROGRAMB based on call type. */
			arguments("exportCallChainWithMultipleCallTypes", parameters(CALL_TYPE, createList("Includes", "Calls"))),
			
			/* Expects a graph of callchain edges from PROGRAMD and checks the type for which the Call Chains must be exported */
			arguments("exportCallChainWithStartModuleIdAndType", parameters(START_MODULE_TYPES, createList(getModuleType("ROOTJOB")), START_MODULE_IDS,
					createList(getModuleId("PROGRAMD")))),
			
			/* Expects a graph of callchain edges for the matching "startModuleTypes". */
			arguments("exportCallChainWithOnlyStartModuleType", parameters(START_MODULE_TYPES, createList(getModuleType("ROOTJOB.STEPA")))),
			
			/* Creates a graph of incoming callchain edges to FILEA till the end module. */
			arguments("exportCallChainEndModuleIdDirectionIn", parameters(START_MODULE_IDS,
					createList(getModuleId("FILEA")), END_MODULE_IDS, createList(getModuleId("ROOTJOB.STEPB")), DIRECTIONS, createList("IN"))),
			
			/* checks for end module type - otherwise call chain would stop immediately if start module matches */
			arguments("exportCallChainEndModuleType", parameters(END_MODULE_TYPES, createList("FILE"), START_MODULE_IDS,
					createList(getModuleId("ROOTJOB")))),
			
			/* checks that first module is ignored in check for end module type - otherwise call chain would stop immediately if start module matches */
			arguments("exportCallChainEndModuleTypeFirstModule", parameters(END_MODULE_TYPES, createList("JOB", "EXEC_PGM"), START_MODULE_IDS,
					createList(getModuleId("ROOTJOB")))),
			
			/* Creates a graph for incoming edges from FILEA and check for end module type for which to stop the export*/
			arguments("exportCallChainEndModuleTypeDirectionIn", parameters(END_MODULE_TYPES, createList("PROGRAM"), START_MODULE_IDS,
					createList(getModuleId("FILEA")), DIRECTIONS, createList("IN"))),
			
			/* We expect a graph having callchains from ROOTJOB to only one node as the specified depth is 1. */
			arguments("exportCallChainDepth1", parameters(START_MODULE_IDS, createList(getModuleId("ROOTJOB")), DEPTH, createList("1"))),
			
			/* Creates a graph for incoming edges from UNLOAD and COPYFILE and check for end module type for which to stop the export and
			 * hide callchain containing Steps, Programs and Files.*/
			arguments("exportCallChainWithFilters", parameters(START_MODULE_IDS, createList(getModuleId("UNLOAD"), getModuleId("COPYFILE")), DIRECTIONS, 
					createList("IN"), FILTERED_TYPE, createList("EXEC_PGM", "PROC", "EXEC"), END_MODULE_TYPES, createList("JOB")))
			);
	}
	
	private void assignTaxonomy(final UUID moduleId, final EntityId taxonomyId) {
		taxonomyService.createModuleLink(moduleId, taxonomyId);
	}
	
	private EntityId createTaxonomy(final UUID taxonomyType, final String taxonomyName) {
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName(taxonomyName)
				.setProject(TEST_PROJECT_ID)
				.setType(taxonomyType);
		return taxonomyService.create(taxonomy);
	}
	
	private static List<String> createList(final String... elements) {
		return Arrays.asList(elements);
	}

	private String getModuleId(final String moduleName) {
		return testData.get(moduleName).getId().toString();
	}
	
	private String getModuleType(final String moduleName) {
		return testData.get(moduleName).getType().name();
	}

	@NonNull
	String moduleId(final TestData testData, final String moduleName) {
		return testData.get(moduleName).getId().toString();
	}

	TestData createTestDataForWMIN8413() {

		/* 
		 * Start--+-->P1---->End
		 *        |
		 *        +-->P2---->End
		 */

		final TestData testData = new TestData();
		final ModulePojo StartModule = createModule("Start", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo Program1 = createModule("P1", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo Program2 = createModule("P2", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo EndModuleA = createModule("End", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/End_Module_A.cbl");
		final ModulePojo EndModuleB = createModule("End", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/End_Module_B.cbl");

		createReference(RelationshipType.CALLS, StartModule.identity(), Program1.identity());
		createReference(RelationshipType.CALLS, StartModule.identity(), Program2.identity());
		createReference(RelationshipType.CALLS, Program1.identity(), EndModuleA.identity());
		createReference(RelationshipType.CALLS, Program2.identity(), EndModuleB.identity());

		testData.add(StartModule);
		testData.add(Program1);
		testData.add(Program2);
		testData.add("EndA", EndModuleA);
		testData.add("EndB", EndModuleB);

		return testData;
	}
	
	TestData createTestDataConditionalDependencyOneModule() {
		final TestData data = new TestData();
		/* Test data for testing conditional dependencies, the full graph would look like this:
		 * 
		 *  JOB_A---->JOB_A.step01--+-->PROG_A--{conditional_modules[JOB_A]}-->FILE
		 *                          |
		 *  JOB_B---->JOB_B.step01--+
		 */
		final ModulePojo jobA = createModule("JOB_A", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo stepA1 = createModule("JOB_A.step01", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		final ModulePojo jobB = createModule("JOB_B", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo stepB1 = createModule("JOB_B.step01", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		final ModulePojo progA = createModule("PROG_A", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo file = createModule("ConditionalFile", Technology.RESOURCE, Type.FILE, Storage.FILE);
		createReference(RelationshipType.CALLS, jobA.identity(), stepA1.identity());
		createReference(RelationshipType.CALLS, jobB.identity(), stepB1.identity());
		createReference(RelationshipType.CALLS, stepA1.identity(), progA.identity());
		createReference(RelationshipType.CALLS, stepB1.identity(), progA.identity());
		final List<EntityId> conditionalDependencyModules = new ArrayList<>();
		conditionalDependencyModules.add(jobA.identity());
		createReference(RelationshipType.ACCESSES, progA.identity(), file.identity(), conditionalDependencyModules);
		
		data.add(jobA);
		data.add(stepA1);
		data.add(jobB);
		data.add(stepB1);
		data.add(progA);
		data.add(file);
		return data;
	}
	
	TestData createTestDataConditionalDependencyWithCommonProc() {
		final TestData data = new TestData();
		/* Test data for testing conditional dependencies, the full graph would look like this:
		 * 
		 *  JOB_A--+-->JOB_A.STEPB.EXEC--+-->APROC--> APROC.PSTEP.EXEC_PGM---+--{conditional[JOB_A.STEPA.EXEC]}-->STEPAPGM
		 *         |                     |                                   | 
		 *         +-->JOB_A.STEPA.EXEC--+                                   +--{conditional[JOB_A.STEPB.EXEC]}-->STEPBPGM
		 */
		final ModulePojo jobA = createModule("JOB_A", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo jobAStepB = createModule("JOB_A.STEPB.EXEC", Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
		final ModulePojo jobAStepA = createModule("JOB_A.STEPA.EXEC", Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
		final ModulePojo aProc = createModule("APROC", Technology.JCL, Type.PROC, Storage.FILE_SECTION);
		final ModulePojo aProcPStep = createModule("APROC.PSTEP.EXEC_PGM", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		final ModulePojo stepAPgm = createModule("STEPAPGM", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo stepBPgm = createModule("STEPBPGM", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		createReference(RelationshipType.CALLS, jobA.identity(), jobAStepB.identity());
		createReference(RelationshipType.CALLS, jobA.identity(), jobAStepA.identity());
		createReference(RelationshipType.CALLS, jobAStepB.identity(), aProc.identity());
		createReference(RelationshipType.CALLS, jobAStepA.identity(), aProc.identity());
		createReference(RelationshipType.CALLS, aProc.identity(), aProcPStep.identity());
		final List<EntityId> condDependencyAPROC_PSTEPToSTEPAPGM = new ArrayList<>();
		condDependencyAPROC_PSTEPToSTEPAPGM.add(jobAStepA.identity());
		createReference(RelationshipType.REFERENCES, aProcPStep.identity(), stepAPgm.identity(), condDependencyAPROC_PSTEPToSTEPAPGM);
		
		final List<EntityId> condDependencyAPROC_PSTEPToSTEPBPGM = new ArrayList<>();
		condDependencyAPROC_PSTEPToSTEPBPGM.add(jobAStepB.identity());
		createReference(RelationshipType.REFERENCES, aProcPStep.identity(), stepBPgm.identity(), condDependencyAPROC_PSTEPToSTEPBPGM);
		
		data.add(jobA);
		data.add(jobAStepB);
		data.add(jobAStepA);
		data.add(aProc);
		data.add(aProcPStep);
		data.add(stepAPgm);
		data.add(stepBPgm);
		return data;
	}
	
	TestData createTestDataMultipleConditionalDependencies() {
		final TestData data = new TestData();
		/* Test data for testing conditional dependencies with multiple required modules, the full graph would look like this:
		 * 
		 *  JOB_A---->JOB_A.step01--+-->PROG_A--{conditional_modules[JOB_A, JOB_B]}-->PROG_B--{conditional_modules[JOB_A]}-->FILE
		 *                          |
		 *  JOB_B---->JOB_B.step01--+
		 *                          |
		 *  JOB_C---->JOB_C.step01--+
		 */
		final ModulePojo jobA = createModule("JOB_A", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo stepA1 = createModule("JOB_A.step01", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		final ModulePojo jobB = createModule("JOB_B", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo stepB1 = createModule("JOB_B.step01", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		final ModulePojo jobC = createModule("JOB_C", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo stepC1 = createModule("JOB_C.step01", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		final ModulePojo progA = createModule("PROG_A", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo progB = createModule("PROG_B", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo file = createModule("ConditionalFile", Technology.RESOURCE, Type.FILE, Storage.FILE);
		createReference(RelationshipType.CALLS, jobA.identity(), stepA1.identity());
		createReference(RelationshipType.CALLS, jobB.identity(), stepB1.identity());
		createReference(RelationshipType.CALLS, stepA1.identity(), progA.identity());
		createReference(RelationshipType.CALLS, stepB1.identity(), progA.identity());
		createReference(RelationshipType.CALLS, jobC.identity(), stepC1.identity());
		createReference(RelationshipType.CALLS, stepC1.identity(), progA.identity());
		List<EntityId> conditionalDependencyModules = new ArrayList<>();
		conditionalDependencyModules.add(jobA.identity());
		conditionalDependencyModules.add(jobB.identity());
		createReference(RelationshipType.CALLS, progA.identity(), progB.identity(), conditionalDependencyModules);
		conditionalDependencyModules = new ArrayList<>();
		conditionalDependencyModules.add(jobA.identity());
		createReference(RelationshipType.ACCESSES, progB.identity(), file.identity(), conditionalDependencyModules);
		data.add(jobA);
		data.add(stepA1);
		data.add(jobB);
		data.add(stepB1);
		data.add(jobC);
		data.add(stepC1);
		data.add(progA);
		data.add(progB);
		data.add(file);
		return data;
	}
	
	TestData createTestDataOneForWMIN8745() {
		
		/* Test data for testing chain with invalid end modules
		 *   +--->----+---->-------+
		 *   |                     |
		 *  End       p2---------->p1----->start 
		 *            |     |      |
		 *            ^-----+      v
		 *            |            |
		 *            +----<-------+
		 */
		final TestData testData = new TestData();

		final ModulePojo StartModule = createModule("Start", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo Program1 = createModule("P1", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/CRTS/P1.cbl");
		final ModulePojo Program2 = createModule("P2", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/RTS/P1.cbl");
		final ModulePojo EndModule = createModule("End", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/End_Module_A.cbl");

		createReference(RelationshipType.CALLS, Program1.identity(), StartModule.identity());
		createReference(RelationshipType.CALLS, Program2.identity(), Program1.identity());
		createReference(RelationshipType.CALLS, Program1.identity(), Program2.identity());
		createReference(RelationshipType.CALLS, Program2.identity(), Program2.identity());
		createReference(RelationshipType.CALLS, EndModule.identity(), Program1.identity());

		testData.add("Start", StartModule);
		testData.add("P1", Program1);
		testData.add("P2", Program2);
		testData.add("End", EndModule);

		return testData;
	}
	
	TestData createTestDataTwoForWMIN8745() {
		
		/*
		 *                 jobB--->----jobC---->---- -+
		 *                 ^                          |
		 *                 |                          V
		 *  start--->p1--->p2----->progA--->progB---->End
		 *                 |                           ^
		 *                 V                           |
		 *                 |                           |
		 *                 jobA---------->-------------+
		 */
		final TestData testData = new TestData();

		final ModulePojo StartModule = createModule("Start", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo Program1 = createModule("P1", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/CRTS/P1.cbl");
		final ModulePojo Program2 = createModule("P2", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/RTS/P2.cbl");
		final ModulePojo progA = createModule("PROG_A", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/CRTS/PROG_A.cbl");
		final ModulePojo progB = createModule("PROG_B", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/CRTS/PROG_B.cbl");
		final ModulePojo jobA = createModule("JOB_A", Technology.JCL, Type.JOB, Storage.FILE, "programs/CRTS/JOB_A.cbl");
		final ModulePojo jobC = createModule("JOB_C", Technology.JCL, Type.JOB, Storage.FILE, "programs/CRTS/JOB_C.cbl");
		final ModulePojo jobB = createModule("JOB_B", Technology.JCL, Type.JOB, Storage.FILE, "programs/CRTS/JOB_B.cbl");
		final ModulePojo EndModule = createModule("End", Technology.COBOL, Type.PROGRAM, Storage.FILE, "programs/End_Module_A.cbl");

		createReference(RelationshipType.CALLS, StartModule.identity(), Program1.identity());
		createReference(RelationshipType.CALLS, Program1.identity(), Program2.identity());
		createReference(RelationshipType.CALLS, Program2.identity(), progA.identity());
		createReference(RelationshipType.CALLS, progA.identity(), progB.identity());
		createReference(RelationshipType.CALLS, progB.identity(), EndModule.identity());
		createReference(RelationshipType.CALLS, Program2.identity(), jobB.identity());
		createReference(RelationshipType.CALLS, jobB.identity(), jobC.identity());
		createReference(RelationshipType.CALLS, jobC.identity(), EndModule.identity());
		createReference(RelationshipType.CALLS, Program2.identity(), jobA.identity());
		createReference(RelationshipType.CALLS, jobA.identity(), EndModule.identity());

		testData.add("Start", StartModule);
		testData.add("P1", Program1);
		testData.add("P2", Program2);
		testData.add("PROG_A", progA);
		testData.add("PROG_B", progB);
		testData.add("JOB_A", jobA);
		testData.add("JOB_C", jobC);
		testData.add("JOB_B", jobB);
		testData.add("End", EndModule);
		return testData;
	}

	void cleanupTestData(final TestData data) {
		final var modules = data.modules.values().stream().map(ModulePojo::getUid).collect(Collectors.toList());

		final Iterator<ModuleRelationshipPojo> iterator = moduleService.findRelationship(q -> q.ofProject(TEST_PROJECT_ID)
																							 .ofModulesInDirection(modules, RelationshipDirection.BOTH)
																							 .withTypes(RelationshipType.DEPENDENCY_TYPES))
																			.iterator();
		while (iterator.hasNext()) {
			final ModuleRelationshipPojo reference = iterator.next();
			moduleService.deleteRelationship(q -> q.byId((reference.getId())));
		}

		moduleService.deleteModules(b -> b.byUids(modules));
	}

	Map<String, List<String>> createParametersWithEndModuleIds(final TestData data, final List<String> endModuleIds){
		Map<String, List<String>> parameters = parameters(START_MODULE_IDS, singletonList(moduleId(data, "Start")), DIRECTIONS, asList("IN", "OUT"),
				END_MODULE_IDS, endModuleIds);
		return parameters;
	}

	@SuppressWarnings("unchecked")
	protected static Map<String, List<String>> parameters(final Object... keyValueParameters) {
		assertEquals(0, keyValueParameters.length % 2, "Parameters must be pairs of key and value");

		final Map<String, List<String>> parameters = new HashMap<>();

		for (int i = 0; i < keyValueParameters.length; i+= 2) {
			assertTrue(keyValueParameters[i] instanceof String, "Key must be an instance of String");
			
			if (keyValueParameters[i + 1] instanceof List) {
				parameters.put((String) keyValueParameters[i], (List<String>) keyValueParameters[i + 1]);
			} else {
				assertTrue(keyValueParameters[i + 1] instanceof String, "Value must be an instance of String");
				parameters.put((String) keyValueParameters[i], Collections.singletonList((String) keyValueParameters[i + 1]));
			}
		}

		return parameters;
	}
}
