/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.util.ModuleFilterUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Test class for testing {@link SearchOrders} and 'search path override' functionality in methods
 * {@code DbModuleRepository.getEntry(ModelArtifact, String, ResolveTarget...)} and
 * {@code DbModuleRepository.getEntry(ModelArtifact, Optional<String>, String, ResolveTarget...)}
 */
@WithMockUser
class Wmin5419Test extends BaseDiscoveryTest {

	private static final String PACKAGE_A1 = "src/jcl/WMIN5419/jcl/a1/**/*";
	private static final String PACKAGE_A2 = "src/jcl/WMIN5419/jcl/a2/**/*";
	private static final String PACKAGE_A3 = "src/jcl/WMIN5419/jcl/a3/**/*";
	private static final String PACKAGE_B = "src/jcl/WMIN5419/jcl/b/**/*";

	@Autowired
	private ArtifactCacheService cacheService;
	@Autowired
	private ModuleFilterUtil moduleFilterUtil;

	@Override
	protected String getTestFolder() {
		return "WMIN5419";
	}

	private ProjectPojo prepare() {
		final var project = createProject();
		final var projectId = project.identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		cacheService.cache.invalidateAll();
		return project;
	}
	
	/**
	 * Tests the default behavior when only one or multiple modules exist for each name using the default {@link SearchOrders} and without specifying a
	 * 'search path override'.
	 */
	@Test
	void testModulesWithDefaultSearchOrders() {
		final ProjectPojo project = prepare();
		final DbModuleRepository repo = createRepo(project.identity(), project.getSearchOrders());

		final LazyModelArtifact jobA1_a1 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a1/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA3_a3 = getModelArtifact(project.identity(), "PROC_A3", "src/jcl/WMIN5419/jcl/a3/procs/PROC_A3.proc", Type.PROC);
		final LazyModelArtifact jobB1 = getModelArtifact(project.identity(), "JOB_B1", "src/jcl/WMIN5419/jcl/b/jobs/JOB_B1.job", Type.JOB);
		final LazyModelArtifact procB1 = getModelArtifact(project.identity(), "PROC_B1", "src/jcl/WMIN5419/jcl/b/procs/PROC_B1.proc", Type.PROC);
		final LazyModelArtifact jobC1 = getModelArtifact(project.identity(), "JOB_C1", "src/jcl/WMIN5419/jcl/c/jobs/JOB_C1.job", Type.JOB);

		/* Only one PROC_A3.proc exists in package a3. So find it even when using JOB_A1.job from package a1 */
		assertMatch(repo, jobA1_a1, procA3_a3);

		/* Find modules that are located in the same package */
		assertMatch(repo, jobB1, procB1);
		/* Previous call caused that the result got cached. Get it from cache again and test the module */
		assertMatch(repo, jobB1, procB1);

		/* Find modules that are located in other packages */
		assertMatch(repo, jobB1, jobC1);
		assertMatch(repo, jobC1, jobB1);

		/* No match for module that has an unknown name */
		final LazyModelArtifact unknown = new LazyModelArtifact(jobB1);
		unknown.setName("unknown");
		unknown.setType(ResolveTarget.ASSEMBLER);
		assertNoMatch(repo, jobB1, unknown);
	}

	/**
	 * Tests the default behavior when only one or multiple modules exist for each name using a custom {@link SearchOrders} and without specifying a
	 * 'search path override'.
	 */
	@Test
	void testModulesWithCustomSearchOrders() {
		final ProjectPojo project = prepare();
		final DbModuleRepository repo = createRepo(project.identity(), createPackageSearchOrder());

		final LazyModelArtifact jobA1_a1 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a1/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA3_a3 = getModelArtifact(project.identity(), "PROC_A3", "src/jcl/WMIN5419/jcl/a3/procs/PROC_A3.proc", Type.PROC);
		final LazyModelArtifact jobB1 = getModelArtifact(project.identity(), "JOB_B1", "src/jcl/WMIN5419/jcl/b/jobs/JOB_B1.job", Type.JOB);
		final LazyModelArtifact procB1 = getModelArtifact(project.identity(), "PROC_B1", "src/jcl/WMIN5419/jcl/b/procs/PROC_B1.proc", Type.PROC);
		final LazyModelArtifact jobC1 = getModelArtifact(project.identity(), "JOB_C1", "src/jcl/WMIN5419/jcl/c/jobs/JOB_C1.job", Type.JOB);

		/* Only one PROC_A3.proc exists in package a3. So find it even when using JOB_A1.job from package a1 */
		assertMatch(repo, jobA1_a1, procA3_a3);
		
		/* Find modules that are located in the same package */
		assertMatch(repo, jobB1, procB1);
		/* Previous call caused that the result got cached. Get it from cache again and test the module */
		assertMatch(repo, jobB1, procB1);

		/* Find modules that are located in other packages */
		assertMatch(repo, jobB1, jobC1);
		assertMatch(repo, jobC1, jobB1);

		/* No match for module that has an unknown name */
		final LazyModelArtifact unknown = new LazyModelArtifact(jobB1);
		unknown.setName("unknown");
		unknown.setType(ResolveTarget.ASSEMBLER);
		assertNoMatch(repo, jobB1, unknown);
	}

	/**
	 * Tests the default behavior when only one or multiple modules exist for each name using the default {@link SearchOrders} but specifying a
	 * 'search path override' which means that the target module is searched for at the given search path first.
	 */
	@Test
	void testModulesWithDefaultSearchOrderAndSearchPathOverride() {
		final ProjectPojo project = prepare();
		final DbModuleRepository repo = createRepo(project.identity(), project.getSearchOrders());

		final LazyModelArtifact jobA1_a1 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a1/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA1_a1 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a1/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA2_a2 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A2.proc", Type.PROC);

		/* Find only a single match when package 'a1' is set as 'search path override' */
		assertMatch(repo, jobA1_a1, procA1_a1, Optional.of(PACKAGE_A1));
		/* Result is not served from cache as no 'search path override' was set. Multiple matches must be returned then */
		assertMultipleMatches(repo, jobA1_a1, procA1_a1);
		
		/* Find only a single match when package 'a2' is set as 'search path override' */
		assertMatch(repo, jobA1_a1, procA2_a2, Optional.of(PACKAGE_A2));

		/* Find only a single match when exact path is set as 'search path override' */
		assertMatch(repo, jobA1_a1, procA2_a2, procA2_a2.getPath());

		/* Result is not served from cache as no 'search path override' was set. Multiple matches must be returned then */
		assertMultipleMatches(repo, jobA1_a1, procA2_a2);

		/* Find multiple matches when package 'b' is set as 'search path override', as there is no module with name 'PROC_A2.proc' in that package */
		assertMultipleMatches(repo, jobA1_a1, procA2_a2, Optional.of(PACKAGE_B));
		/* No caching again. Find match if no 'search path override' is set. */
		assertMultipleMatches(repo, jobA1_a1, procA2_a2);

		/* No match for module that has an unknown name */
		final LazyModelArtifact unknown = new LazyModelArtifact(jobA1_a1);
		unknown.setName("unknown");
		unknown.setType(ResolveTarget.ASSEMBLER);
		assertNoMatch(repo, jobA1_a1, unknown);
	}

	/**
	 * Tests the default behavior when only one or multiple modules exist for each name using a custom {@link SearchOrders} but specifying a
	 * 'search path override' which means that the target module is searched for at the given search path first.
	 */
	@Test
	void testModulesWithPackageSearchOrderAndSearchPathOverride() {
		final ProjectPojo project = prepare();
		final DbModuleRepository repo = createRepo(project.identity(), createPackageSearchOrder());

		final LazyModelArtifact jobA1_a1 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a1/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact jobA1_a3 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a3/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA1_a1 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a1/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA1_a2 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA2_a2 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A2.proc", Type.PROC);
		final LazyModelArtifact procA2_a3 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a3/procs/PROC_A2.proc", Type.PROC);

		/* For all a3's the SearchOrders is setup so the repo first searches in 'a2' and then in 'a3' package */
		assertMatch(repo, jobA1_a3, procA1_a2);

		/* Same as before but this time search in package 'a2' first. Result must not be served from cache */
		assertMatch(repo, jobA1_a3, procA2_a2, Optional.of(PACKAGE_A2));

		/* Same as before but this time search in package 'a3' first. Result must not be served from cache as a different 'search path override'
		 * is used so we also test that the 'search path override' is part of the cache key */
		assertMatch(repo, jobA1_a1, procA2_a3, Optional.of(PACKAGE_A3));

		/* Find only a single match when package 'a1' is set as 'search path override' */
		assertMatch(repo, jobA1_a1, procA1_a1, Optional.of(PACKAGE_A1));

		/* Find again and test that result was not served from cache as no 'search path override' was set. So it matches by custom SearchOrders */
		assertMatch(repo, jobA1_a1, procA1_a1);
		
		/* Find only a single match when package 'a2' is set as 'search path override' */
		assertMatch(repo, jobA1_a1, procA2_a2, Optional.of(PACKAGE_A2));

		/* Find only a single match when exact path is set as 'search path override' */
		assertMatch(repo, jobA1_a1, procA2_a2, procA2_a2.getPath());

		/* No match for module that has an unknown name */
		final LazyModelArtifact unknown = new LazyModelArtifact(jobA1_a1);
		unknown.setName("unknown");
		unknown.setType(ResolveTarget.ASSEMBLER);
		assertNoMatch(repo, jobA1_a1, unknown);
	}

	/**
	 * Tests the default behavior when multiple modules exist for each name using the default {@link SearchOrders}.
	 */
	@Test
	void testDuplicateMatchessWithDefaultSearchOrder() {
		final ProjectPojo project = prepare();
		final DbModuleRepository repo = createRepo(project.identity(), project.getSearchOrders());

		final LazyModelArtifact jobA1_a1 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a1/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA1_a1 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a1/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA2_a1 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a1/procs/PROC_A2.proc", Type.PROC);
		final LazyModelArtifact procA1_a2 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA2_a2 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A2.proc", Type.PROC);

		/* Source jobA1_a1 is in jcl/a1/jobs but procA1_a1 is in jcl/a1/procs so no entry must be found */
		assertMultipleMatches(repo, jobA1_a1, procA1_a1);

		/* Source jobA1_a1 is in jcl/a1/jobs but procA2_a1 is in jcl/a1/procs so no entry must be found */
		assertMultipleMatches(repo, jobA1_a1, procA2_a1);

		/* Source procA1_a1 is in the same package as procA2_a1 so procA2_a1 must be found */
		assertMatch(repo, procA1_a1, procA2_a1);

		/* Source procA1_a2 is in same package so procA2_a2 must be found.
		 * In the old implementation the repo would return procA2_a1 that was cached by the previous call. The problem with that
		 * is that the results of the Metrics Discovery are then different, depending on if the repo is called first for procA1_a1
		 * or procA1_a1 */
		assertMatch(repo, procA1_a2, procA2_a2);

		/* No exact match although call with different source before found the correct match
		 * In the old implementation the repo would return procA2_a1 that was cached by the previous call. The problem with that
		 * is that the results of the Metrics Discovery are then different, depending on if the repo is called first for procA1_a1
		 * or procA1_a1 */
		assertMultipleMatches(repo, jobA1_a1, procA2_a1);

		final LazyModelArtifact unknown = new LazyModelArtifact(procA2_a2);
		unknown.setName("unknown");
		unknown.setType(ResolveTarget.ASSEMBLER);
		assertNoMatch(repo, procA1_a2, unknown);
	}

	/**
	 * Tests the default behavior when multiple modules exist for each name using a custom {@link SearchOrders}.
	 */
	@Test
	void testDuplicateMatchessWithPackageSearchOrder() {
		final ProjectPojo project = prepare();
		final DbModuleRepository repo = createRepo(project.identity(), createPackageSearchOrder());

		final LazyModelArtifact jobA1_a1 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a1/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA1_a1 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a1/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA2_a1 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a1/procs/PROC_A2.proc", Type.PROC);
		final LazyModelArtifact jobA1_a2 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a2/jobs/JOB_A1.job", Type.JOB);
		final LazyModelArtifact procA1_a2 = getModelArtifact(project.identity(), "PROC_A1", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A1.proc", Type.PROC);
		final LazyModelArtifact procA2_a2 = getModelArtifact(project.identity(), "PROC_A2", "src/jcl/WMIN5419/jcl/a2/procs/PROC_A2.proc", Type.PROC);
		final LazyModelArtifact jobA1_a3 = getModelArtifact(project.identity(), "JOB_A1", "src/jcl/WMIN5419/jcl/a3/jobs/JOB_A1.job", Type.JOB);

		/* For all a1's first search in a1 package */
		assertMatch(repo, jobA1_a1, procA1_a1);
		assertMatch(repo, jobA1_a1, procA2_a1);

		/* For all a2's first search in a2 package */
		assertMatch(repo, jobA1_a2, procA1_a2);
		assertMatch(repo, jobA1_a2, procA2_a2);

		/* For all a3's first search in a2 and then in a3 package */
		assertMatch(repo, jobA1_a3, procA1_a2);
		assertMatch(repo, jobA1_a3, procA2_a2);
	}

	private static List<SearchOrder> createPackageSearchOrder() {
		final List<SearchOrder> searchOrders = new ArrayList<>();
		/* For all a1's first search in a1 package */
		searchOrders.add(SearchOrder.fromPatterns(PACKAGE_A1, (PACKAGE_A1)));
		/* For all a2's first search in a2 package */
		searchOrders.add(SearchOrder.fromPatterns(PACKAGE_A2, PACKAGE_A2));
		/* For all a3's first search in a2 and then in a3 package */
		searchOrders.add(SearchOrder.fromPatterns(PACKAGE_A3, PACKAGE_A2, PACKAGE_A3));
		/* Default */
		searchOrders.add(SearchOrder.fromPatterns("**/*", "./*", "**/*"));

		return searchOrders;
	}

	private LazyModelArtifact getModelArtifact(final EntityId projectId, final String name, final String path, final Type type) {
		final List<LazyModelArtifact> matches = modelArtifactService.find(b -> b.ofProject(projectId)
																				.withName(name, true)
																				.withPathsSelfOrContaining(path, true)
																				.withTechnologiesAndTypes(List.of(Tuple2.of(Technology.JCL, type))));

		assertEquals(1, matches.size(), String.format("Exactly one '%s' module with path '%s' and type '%S' must exists", name, path, type));
		return matches.get(0);
	}

	private static void assertMatch(final DbModuleRepository repo, final LazyModelArtifact source, final LazyModelArtifact target) {
		assertMatch(repo, source, target, Optional.empty());
	}

	private static void assertMatch(final DbModuleRepository repo, final LazyModelArtifact source, final LazyModelArtifact target, final Optional<String> searchPathOverride) {
		final Long errors = source.getErrors().collect(Collectors.counting());
		final Optional<ModelArtifact> entry = searchPathOverride.isPresent() ?
													repo.getEntry(source, searchPathOverride, target.getName(), target.getType()) : 
													repo.getEntry(source, target.getName(), target.getType()); 
		assertEquals(errors, source.getErrors().collect(Collectors.counting()), "repo.getEntry() must not return with errors");
		
		assertTrue(entry.isPresent(), String.format("Module '%s' must be found when search with source '%s'", target.getName(), source.getPath()));
		
		final ModelArtifact targetFound = entry.get();
		assertTrue(targetFound.getPath().isPresent(), "Path must be present in found module");

		assertEquals(target.getPath(), targetFound.getPath(), "Path of found module must match with target");
	}

	private static void assertMultipleMatches(final DbModuleRepository repo, final LazyModelArtifact source, final LazyModelArtifact target) {
		assertMultipleMatches(repo, source, target, Optional.empty());
	}
		
	private static void assertMultipleMatches(final DbModuleRepository repo, final LazyModelArtifact source, final LazyModelArtifact target,
											  final Optional<String> searchPathOverride) {
		final long errors = source.getAddedErrors().count();
		final Optional<ModelArtifact> entry = searchPathOverride.isPresent() ?
													repo.getEntry(source, searchPathOverride, target.getName(), target.getType()) :
													repo.getEntry(source, target.getName(), target.getType());
		assertFalse(entry.isPresent(), String.format("Module '%s' must NOT be found when search with source '%s'", target.getName(), source.getPath()));

		assertEquals(errors + 1, source.getAddedErrors().count());

		final int[] checked = { 0 };
		final ResolveTarget[] types = { target.getType() };
		final String expectedError = String.format(IModuleRepository.ERROR_MULTIPE_CANDIDATES, target.getName(), Arrays.toString(types), "");
		source.getAddedErrors().skip(errors).forEach(error -> {
			checked[0]++;
			assertTrue(error.getCause().startsWith(expectedError), "Error must start with: " + expectedError);
		});

		assertEquals(1, checked[0], "Exactly one ModelError must have been checked");
	}

	private static void assertNoMatch(final DbModuleRepository repo, final LazyModelArtifact source, final ModelArtifact target) {
		final Long errors = source.getErrors().collect(Collectors.counting());
		final Optional<ModelArtifact> entry = repo.getEntry(source, target.getName(), target.getType());
		assertFalse(entry.isPresent(), String.format("Module '%s' must NOT be found when search with source '%s'", target.getName(), source.getPath()));
		assertEquals(errors, source.getErrors().collect(Collectors.counting()));
	}
	
	private DbModuleRepository createRepo(final EntityId projectId, final List<SearchOrder> searchOrders) {
		return new DbModuleRepository(projectId, modelArtifactService, "JOB_A123", cacheService, new SearchOrders(searchOrders), moduleFilterUtil);
	}
}
