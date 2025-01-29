package innowake.mining.server.discovery.dawn.metrics.impl.service;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Instant;
import java.util.Collections;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Tests to verify that the ErrorMarkers are being added when tasks are failed.
 */
class DawnDiscoveryTaskErrorsTest extends DatabaseRelatedTest {

	@Autowired
	private AutowireCapableBeanFactory autowireCapableBeanFactory;

	private EntityId projectId = EntityId.VOID;

	@Mock
	@Nullable
	private JobMonitor jobMonitor;

	private final ModuleParameters moduleParameters = new ModuleParameters(Instant.now());

	@Autowired
	private ModuleService moduleService;

	@Nullable
	private ContributorResult contributorResult;

	@BeforeAll
	void initialize() {
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Test Project");
		project.setNatures(Collections.emptySet());
		project.setClient(EntityId.of(1L));
		projectId = projectService.create(project).identity();

		final ModulePojoPrototype moduleDefinition = new ModulePojoPrototype()
				.setName("subModule1")
				.setTechnology(ModuleType.ASSEMBLER_PROGRAM.getTechnology())
				.setType(ModuleType.ASSEMBLER_PROGRAM.getType())
				.setRepresentation(Representation.VIRTUAL)
				.setStorage(Storage.FILE_SECTION)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM);

		contributorResult = new ContributorResult(ContributorResult.Type.ROOT_MODULE,
				new ModuleFilter().setNames("Module1"),
				Collections.emptySet(), moduleDefinition, Collections.emptyList(),
				Collections.emptyList(), Collections.emptyList(), Collections.emptyList(),
				Collections.emptyList());
	}
	
	@BeforeEach
	void resetErrors() {
		moduleService.deleteErrorMarkers(q -> q.ofProject(projectId));
	}

	@Test
	void testAnchorTaskError() {
		final var nonNullJobMonitor = assertNotNull(jobMonitor);
		final AnchorTask task = new AnchorTask(nonNullJobMonitor, projectId, 0, moduleParameters, assertNotNull(contributorResult));
		autowireCapableBeanFactory.autowireBean(task);
		final var exception = assertThrows(IllegalStateException.class, () -> task.run(nonNullJobMonitor));
		final var expectedMessage = "Error occurred while executing anchor task with module filter: \n" + "names: [Module1]\n";
		final var actualMessage = exception.getMessage();
		assertTrue(actualMessage.contains(expectedMessage));
		final var errorMarkers = moduleService.findErrorMarkers(q -> q.ofProject(projectId));
		assertEquals(1, errorMarkers.size());
		assertEqualsErrorMarker(errorMarkers.get(0), expectedMessage);
	}

	@Test
	void testContributorTaskError() {
		final var nonNullJobMonitor = assertNotNull(jobMonitor);
		final ContributorTask task = new ContributorTask(nonNullJobMonitor, projectId, 0, moduleParameters, "HelloDawn");
		autowireCapableBeanFactory.autowireBean(task);
		final var exception = assertThrows(IllegalStateException.class, () -> task.run(nonNullJobMonitor));
		final var expectedMessage = "ContributorTask: Unable to execute contributor: Contributor class HelloDawn not found.";
		final var actualMessage = exception.getMessage();
		assertTrue(actualMessage.contains(expectedMessage));
		final var errorMarkers = moduleService.findErrorMarkers(q -> q.ofProject(projectId));
		assertEquals(1, errorMarkers.size());
		assertEqualsErrorMarker(errorMarkers.get(0), expectedMessage);
	}

	@Test
	void testCreateIfMissingDefaultModulesTaskError() {
		final var nonNullJobMonitor = assertNotNull(jobMonitor);
		final CreateIfMissingDefaultModulesTask task = new CreateIfMissingDefaultModulesTask(nonNullJobMonitor, projectId, 0, moduleParameters,
				assertNotNull(contributorResult));
		autowireCapableBeanFactory.autowireBean(task);
		final var exception = assertThrows(IllegalStateException.class, () -> task.run(nonNullJobMonitor));
		final var expectedMessage ="Error occurred while executing createIfMissing task with module filter: \n" + "names: [Module1]\n";
		final var actualMessage = exception.getMessage();
		assertTrue(actualMessage.contains(expectedMessage));
		final var errorMarkers = moduleService.findErrorMarkers(q -> q.ofProject(projectId));
		assertEquals(1, errorMarkers.size());
		assertEqualsErrorMarker(errorMarkers.get(0), expectedMessage);
	}
	
	private void assertEqualsErrorMarker(final ErrorMarkerPojo errorMarker, final String expectedMessage) {
		assertEquals(Severity.ERROR, errorMarker.getSeverity());
		assertEquals(ErrorKey.MODULE_ABORT, errorMarker.getKey());
		assertTrue(errorMarker.getCause().contains(expectedMessage));
	}

	
}
