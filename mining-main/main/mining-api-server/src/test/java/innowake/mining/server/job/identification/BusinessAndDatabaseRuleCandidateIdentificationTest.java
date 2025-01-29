/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.javatuples.Triplet;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Business Rule and Database Rule Identification job tests
 */

@WithMockUser
class BusinessAndDatabaseRuleCandidateIdentificationTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;

	@Autowired
	private AnnotationService annotationService;

	/**
	 * Tests the business rule candidate identification.
	 */
	@Test
	void testAnnotationRuleCandidatesIdentificationById() {
		final String path = RESOURCE_PATH + "MMRS7101.cbl";
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BusinessRule", "MMRS7101.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue("There must be no annotations present before the Business Rule candidate identification job ran", annotationsBefore.isEmpty());

		/* Run the Business Rule Candidate Identification on the Cobol Module */
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
		
		final List<Triplet<Integer, String, AnnotationType>> expectedAnnotations = new ArrayList<Triplet<Integer, String, AnnotationType>>();
		expectedAnnotations.add(T(4692, "Field Computation Rule Candidate [System identified]", AnnotationType.RULE));
		expectedAnnotations.add(T(4060, "Data Validation Rule Candidate [System identified]", AnnotationType.RULE));
		expectedAnnotations.add(T(6957, "Data Validation Rule Candidate [System identified]", AnnotationType.RULE));
		
		/* Verify that the Cobol Program Module has the business rule candidates associated */
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertFalse("annotationAfter should not be empty", annotationsAfter.isEmpty());

		Collections.sort(annotationsAfter, (o1, o2) -> o1.getLocation().map(ModuleLocation::getOffset).orElse(0) - o2.getLocation().map(ModuleLocation::getOffset).orElse(0));
		Collections.sort(expectedAnnotations, (o1, o2) -> o1.getValue0() - o2.getValue0());

		assertResults(annotationsAfter, expectedAnnotations);
	}

	/**
	 * Tests the business rule candidate identification.
	 */
	@Test
	void testAnnotationRuleCandidatesIdentificationByPath() {
		final String path = RESOURCE_PATH + "MMRS7101.cbl";
		createCobolProgram(PROJECT_ID_2, "BusinessRule", "MMRS7101.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_2).withModulePath(path));
		assertTrue("There must be no annotations present before the Business Rule candidate identification job ran", annotationsBefore.isEmpty());

		/* Run the Business Rule Candidate Identification on the Cobol Module */
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_2, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
		
		final List<Triplet<Integer, String, AnnotationType>> expectedAnnotations = new ArrayList<Triplet<Integer, String, AnnotationType>>();
		expectedAnnotations.add(T(Integer.valueOf(4692), "Field Computation Rule Candidate [System identified]", AnnotationType.RULE));
		expectedAnnotations.add(T(Integer.valueOf(4060), "Data Validation Rule Candidate [System identified]", AnnotationType.RULE));
		expectedAnnotations.add(T(Integer.valueOf(6957), "Data Validation Rule Candidate [System identified]", AnnotationType.RULE));

		/* Verify that the Cobol Program Module has the business rule candidates associated */
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_2).withModulePath(path));
		assertFalse("annotationAfter should not be empty", annotationsAfter.isEmpty());

		assertResults(annotationsAfter, expectedAnnotations);
	}

	/**
	 * Tests the database rule candidate identification.
	 */
	@Test
	void testDatabaseRuleCandidatesIdentificationById() {
		final String path = RESOURCE_PATH + "DBF.cbl";
		final EntityId cobolId = createCobolProgram(PROJECT_ID_1, "Database", "DBF.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue("There must be no annotations present before the database rule candidate identification job ran", annotationsBefore.isEmpty());

		/* Run the Database Rule Candidate Identification on the Cobol Module */
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(cobolId), Collections.emptyList())));

		final List<Triplet<Integer, String, AnnotationType>> expectedAnnotations = new ArrayList<Triplet<Integer, String, AnnotationType>>();
		expectedAnnotations.add(T(668, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(820, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(1281, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(1433, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(2297, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(2610, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(2933, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(3106, "System identified Database Query", AnnotationType.DATABASE));
		
		/* Verify that the Cobol Program Module has the database rule candidates associated */
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertFalse("annotationAfter should not be empty", annotationsAfter.isEmpty());

		annotationsAfter.sort((t1, t2) -> t1.getId().compareTo(t2.getId()));
		expectedAnnotations.sort((t1, t2) -> t1.getValue0().compareTo(t2.getValue0()));
		assertResults(annotationsAfter, expectedAnnotations);
	}
	/**
	 * Tests the database rule candidate identification.
	 */
	@Test
	void testDatabaseRuleCandidatesIdentificationByPath() {
		final String path = RESOURCE_PATH + "DBF.cbl";
		createCobolProgram(PROJECT_ID_2, "Database", "DBF.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_2).withModulePath(path));
		assertTrue("There must be no annotations present before the database rule candidate identification job ran", annotationsBefore.isEmpty());

		/* Run the Database Rule Candidate Identification on the Cobol Module */
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_2, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));

		final List<Triplet<Integer, String, AnnotationType>> expectedAnnotations = new ArrayList<Triplet<Integer, String, AnnotationType>>();
		expectedAnnotations.add(T(668, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(820, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(1281, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(1433, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(2297, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(2610, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(2933, "System identified Database Query", AnnotationType.DATABASE));
		expectedAnnotations.add(T(3106, "System identified Database Query", AnnotationType.DATABASE));

		/* Verify that the Cobol Program Module has the database rule candidates associated */
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_2).withModulePath(path));
		assertFalse("annotationAfter should not be empty", annotationsAfter.isEmpty());
		assertEquals(4,
				annotationsAfter.stream()
						.filter(annotation -> innowake.lib.core.lang.Assert.assertNotNull(annotation.getCategoryName().orElse(null))
								.equals(AnnotationCategory.DatabaseAnnotationCategory.READ.getName()))
						.count(),
				"Number of Read category for annotation should be correct");
		assertEquals(2,
				annotationsAfter.stream()
						.filter(annotation -> innowake.lib.core.lang.Assert.assertNotNull(annotation.getCategoryName().orElse(null))
								.equals(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()))
						.count(),
				"Number of Close category for annotation should be correct");

		assertResults(annotationsAfter, expectedAnnotations);
	}
	
	/**
	 * @param offset The offset value
	 * @param name The expected name value
	 * @param type The expected type
	 * @return A {@link Triplet} with the {@code offset}, {@code name} and {@code type)}
	 */
	protected static final Triplet<Integer, String, AnnotationType> T(final int offset, final String name, final AnnotationType type) {
		return Triplet.with(Integer.valueOf(offset), name, type);
	}
	
	private void assertResults(final List<AnnotationPojo> annotationsAfter, final List<Triplet<Integer, String, AnnotationType>> expectedAnnotations) {
		final var expected = expectedAnnotations.stream()
											.map(t -> "Offset: " + t.getValue0() + ", Name: " + t.getValue1() + ", Type: " + t.getValue2())
											.collect(Collectors.toSet());

		for (int i = 0; i < annotationsAfter.size(); i++) {
			final AnnotationPojo annotation = annotationsAfter.get(i);
			final Optional<ModuleLocation> location = annotation.getLocation();
			assertTrue("ModuleLocation must be present in annotation", location.isPresent());
			final String key = "Offset: " + location.get().getOffset() + ", Name: " + annotation.getName() + ", Type: " + annotation.getType();
			assertTrue("Business Rule candidate annotation must match", expected.contains(key));
		}
	}
}
