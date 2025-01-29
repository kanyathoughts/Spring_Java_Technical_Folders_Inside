/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.extensions.export.callchain;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.*;
import org.apache.commons.lang3.time.StopWatch;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.extensions.export.callchain.model.CallChain;

/**
 * Performance Test for CallChain CSV Export job
 */
@Disabled("Long running performance tests for manual execution")
@TestInstance(Lifecycle.PER_CLASS) /* required for access to testData */
@WithMockUser
class CallChainCsvExportPerformanceTest extends AbstractCallChainExporterTest {
	
	private static final Logger LOG = LoggerFactory.getLogger(CallChainCsvExportPerformanceTest.class);
	private Long JOB_ID = Long.valueOf(0);
	
	@ParameterizedTest
	@CsvSource({
		"2, 17",
		"32, 257",
		"1024, 8193",
		"2000, 16001"
	})
	void testCallChainPerformanceTest(final int iterations, final int expectedModules) {
		final TestData data = createData(iterations);
		final int totalModules = data.modules.size();
		assertEquals(expectedModules, totalModules);
		
		/* Running Call Chain job involving 17 modules took 00:00:00.609 */
		/* Running Call Chain job involving 257 modules took 00:00:00.767 */
		/* Running Call Chain job involving 8193 modules took 00:00:04.280 */
		/* Running Call Chain job involving 16001 modules took 00:00:06.707 */
		
		time(() -> {
			callChainJob(getParameters());
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, "Running Call Chain job involving " + totalModules + " modules");
		LOG.info("Started Cleaning Test Data");
		cleanupTestData(data);
		LOG.info("Test Data cleanup completed");
	}
	
	@ParameterizedTest
	@CsvSource({
		"100, 303",
		"1000, 3003",
		"5000, 15003"
	})
	void testCallChainExportWithMultipleStepsCalling1Proc(final int iterations, final int expectedModules) {
		final TestData data = createDataWithMultipleStepsSingleProc(iterations);
		final int totalModules = data.modules.size();
		assertEquals(expectedModules, totalModules);
		/* 100 steps - Running Call Chain job involving 303 modules took 00:00:00.806 */
		/* 1000 steps - Running Call Chain job involving 3003 modules took 00:00:02.353 */
		/* 5000 steps - Running Call Chain job involving 15003 modules took 00:00:28.639 */
		
		time(() -> {
			callChainJob(getParameters());
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, "Running Call Chain job involving " + totalModules + " modules");
		LOG.info("Started Cleaning Test Data");
		cleanupTestData(data);
		LOG.info("Test Data cleanup completed");
	}
	
	@ParameterizedTest
	@CsvSource({
		"100, 404",
		"1000, 4004"
	})
	void testCallChainExportWithMultipleJobs(final int iterations, final int expectedModules) {
		final TestData data = createDataWithMultipleJobs(iterations);
		final int totalModules = data.modules.size();
		assertEquals(expectedModules, totalModules);
		/* 100 iterations - Running Call Chain job involving 404 modules took 00:00:00.782 */
		/* 1000 iterations - Running Call Chain job involving 4004 modules took 00:00:01.709 */
		
		time(() -> {
			callChainJob(getParameters());
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, "Running Call Chain job involving " + totalModules + " modules");
		LOG.info("Started Cleaning Test Data");
		cleanupTestData(data);
		LOG.info("Test Data cleanup completed");
	}
	
	private TestData createData(final int maxIteration) {
		/* Test data for testing conditional dependencies, the full graph would look like this:
		 * 
		 * 
		 *  JOBA--+-->JOBA.STEP1--+-->PROC1--> JOBA.STEP.EXEC_PGM1---+--{conditional[JOBA.STEP1]}-->FILE1 & MODULE1
		 *         |              |                                 | 
		 *         +->JOBA.STEP2--+                                 +--{conditional[JOBA.STEP2]}-->FILE2 & MODULE2
		 *         |
		 *         +->JOBA.STEP3--+-->PROC3--> JOBA.STEP.EXEC_PGM3---+--{conditional[JOBA.STEP3]}-->FILE3 & MODULE3
		 *         |              |                                 | 
		 *         +->JOBA.STEP4--+                                 +--{conditional[JOBA.STEP4]}-->FILE3 & MODULE4
		 *         |
		 *         |
		 *         +->JOBA.STEPn--+-->PROCn--> JOBA.STEP.EXEC_PGMn---+--{conditional[JOBA.STEPn]}-->FILEn & MODULEn
		 *         |              |                                 | 
		 *         +->JOBA.STEPn+1--+                                 +--{conditional[JOBA.STEPn+1]}-->FILE3n+1 & MODULEn+1
		 */
		LOG.info("Creating Data for Call Chain job");
		final TestData data = new TestData();
		final ModulePojo job = createModule("JOBA", Technology.JCL, Type.JOB, Storage.FILE);
		data.add(job);
		
		for (int i = 1; i <= maxIteration*2; i = i + 2) {
			final ModulePojo step1 = createModule("JOBA.STEP" + i, Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
			final ModulePojo step2 = createModule("JOBA.STEP" + String.valueOf(i + 1), Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
			createReference(RelationshipType.CALLS, job.identity(), step1.identity());
			createReference(RelationshipType.CALLS, job.identity(), step2.identity());
			
			final ModulePojo proc = createModule("PROC" + String.valueOf(i), Technology.JCL, Type.PROC, Storage.FILE_SECTION);
			createReference(RelationshipType.CALLS, step1.identity(), proc.identity());
			createReference(RelationshipType.CALLS, step2.identity(), proc.identity());
			
			final ModulePojo prog = createModule("JOBA.STEP.EXEC_PGM" + String.valueOf(i), Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
			createReference(RelationshipType.CALLS, proc.identity(), prog.identity());
			
			final ModulePojo cobol1 = createModule("MODULE" + String.valueOf(i), Technology.COBOL, Type.PROGRAM, Storage.FILE);
			final ModulePojo cobol2 = createModule("MODULE" + String.valueOf(i + 1), Technology.COBOL, Type.PROGRAM, Storage.FILE);
			createConditionalDependecies(prog, cobol1, step1);
			createConditionalDependecies(prog, cobol2, step2);
			
			final ModulePojo file1 = createModule("FILE" + String.valueOf(i), Technology.RESOURCE, Type.FILE, Storage.FILE);
			final ModulePojo file2 = createModule("FILE" + String.valueOf(i + 1), Technology.RESOURCE, Type.FILE, Storage.FILE);
			createConditionalDependecies(prog, file1, step1);
			createConditionalDependecies(prog, file2, step2);
			
			data.add(step1);
			data.add(step2);
			data.add(proc);
			data.add(prog);
			data.add(cobol1);
			data.add(cobol2);
			data.add(file1);
			data.add(file2);
		}
		LOG.info("Data Created for Call Chain job");
		JOB_ID = job.getId();
		return data;
	}
	
	private TestData createDataWithMultipleStepsSingleProc(final int maxIterations) {
		/* Test data for testing conditional dependencies, the full graph would look like this:
		 * 
		 *  JOBB--+-->JOBB.STEP1--+-->PROCB--> PROCB.EXEC.STEP---+--{conditional[JOBB.STEP1]}-->FILE1 & MODULE1
		 *         |              |                                 | 
		 *         +->JOBB.STEP2--+                                 +--{conditional[JOBB.STEP2]}-->FILE2 & MODULE2
		 *         |              |                                 |
		 *         +->JOBB.STEP3--+                                 +--{conditional[JOBB.STEP3]}-->FILE3 & MODULE3
		 *         |              |                                 | 
		 *         +->JOBB.STEP4--+                                 +--{conditional[JOBB.STEP4]}-->FILE3 & MODULE4
		 *         |              |                                 |
		 *         +->JOBB.STEPn--+                                 +--{conditional[JOBB.STEPn]}-->FILEn & MODULEn
		 */
		LOG.info("Creating Data for Call Chain job");
		final TestData data = new TestData();
		final ModulePojo job = createModule("JOBB", Technology.JCL, Type.JOB, Storage.FILE);
		data.add(job);
		
		final ModulePojo proc = createModule("PROCB", Technology.JCL, Type.PROC, Storage.FILE_SECTION);
		final ModulePojo prog = createModule("PROCB.EXEC.STEP", Technology.JCL, Type.EXEC_PGM, Storage.FILE_SECTION);
		createReference(RelationshipType.CALLS, proc.identity(), prog.identity());
		data.add(proc);
		data.add(prog);
		
		for (int i = 1; i <= maxIterations; i++) {
			final ModulePojo step = createModule("JOBB.STEP" + i, Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
			createReference(RelationshipType.CALLS, job.identity(), step.identity());
			
			createReference(RelationshipType.CALLS, step.identity(), proc.identity());
			
			final ModulePojo cobol = createModule("MODULE" + i, Technology.COBOL, Type.PROGRAM, Storage.FILE);
			createConditionalDependecies(prog, cobol, step);
			
			final ModulePojo file = createModule("FILE" + i, Technology.RESOURCE, Type.FILE, Storage.FILE);
			createConditionalDependecies(prog, file, step);
			
			data.add(step);
			data.add(cobol);
			data.add(file);
		}
		LOG.info("Data Created for Call Chain job");
		JOB_ID = job.getId();
		return data;
	}
	
	private TestData createDataWithMultipleJobs(final int maxIterations) {
		/* Test data for testing conditional dependencies, the full graph would look like this:
		 * 
		 *  JOB1--+-->JOB1.STEP1--+                                 +--{conditional[JOBB.STEP1]}-->JOB1_STEP1PGM
		 *         |              |                                 | 
		 *         +->JOB1.STEP2--+                                 +--{conditional[JOBB.STEP2]}-->JOB1_STEP2PGM
		 *         |              |                                 |
		 *         +->JOBB.STEPn--+                                 +--{conditional[JOBB.STEP3]}-->JOB1_STEPnPGM
		 *                        |-->PROCB--> PROCB.EXEC.STEP---+->|
		 *  JOB2--+-->JOB2.STEP1--+                                 +--{conditional[JOB2.STEP1]}-->JOB2_STEP1PGM
		 *         |              |                                 | 
		 *         +->JOB2.STEP2--+                                 +--{conditional[JOB2.STEP2]}-->JOB2_STEP2PGM
		 *         |              |                                 |
		 *         +->JOB2.STEPn--+                                 +--{conditional[JOB2.STEP3]}-->JOB2_STEPnPGM
		 */
		LOG.info("Creating Data for Call Chain job");
		final TestData data = new TestData();
		final ModulePojo job1 = createModule("JOB1", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo job2 = createModule("JOB2", Technology.JCL, Type.JOB, Storage.FILE);
		data.add(job1);
		data.add(job2);
		
		final ModulePojo proc = createModule("PROC", Technology.JCL, Type.PROC, Storage.FILE_SECTION);
		final ModulePojo procExec = createModule("PROC.EXEC.STEP", Technology.JCL, Type.PROC, Storage.FILE_SECTION);
		createReference(RelationshipType.CALLS, proc.identity(), procExec.identity());
		data.add(proc);
		data.add(procExec);
		
		for (int i = 1; i <= maxIterations; i++) {
			final ModulePojo step1 = createModule("JOB1.STEP" + i, Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
			final ModulePojo step2 = createModule("JOB2.STEP" + i, Technology.JCL, Type.EXEC, Storage.FILE_SECTION);
			createReference(RelationshipType.CALLS, job1.identity(), step1.identity());
			createReference(RelationshipType.CALLS, job2.identity(), step2.identity());
			
			createReference(RelationshipType.CALLS, step1.identity(), proc.identity());
			createReference(RelationshipType.CALLS, step2.identity(), proc.identity());
			
			final ModulePojo prog1 = createModule("JOB1_STEP" + i + "PGM", Technology.COBOL, Type.PROGRAM, Storage.FILE);
			final ModulePojo prog2 = createModule("JOB2_STEP" + i + "PGM", Technology.COBOL, Type.PROGRAM, Storage.FILE);
			
			createConditionalDependecies(procExec, prog1, step1);
			createConditionalDependecies(procExec, prog2, step2);
			
			data.add(step1);
			data.add(prog1);
			data.add(step2);
			data.add(prog2);
		}
		LOG.info("Data Created for Call Chain job");
		JOB_ID = job1.getId();
		return data;
	}
	
	private void createConditionalDependecies(final ModulePojo source, final ModulePojo target, final ModulePojo dependency) {
		final List<EntityId> conditionalDependency = new ArrayList<>();
		conditionalDependency.add(dependency.identity());
		createReference(RelationshipType.REFERENCES, source.identity(), target.identity(), conditionalDependency);
	}
	
	private Map<String, List<String>> getParameters() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(START_MODULE_IDS, Arrays.asList(String.valueOf(JOB_ID)));
		parameters.put(DIRECTIONS, Arrays.asList(CallChain.CallChainDirection.OUT.name()));
		return parameters;
	}
	
	private <V> V time(final Callable<V> runnable, final String message) {
		final StopWatch watch = new StopWatch();
		watch.start();
		final V result;
		try {
			result = runnable.call();
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		watch.stop();
		LOG.info(() -> String.format("%s took %s", message, watch.toString()));
		return result;
	}
}
