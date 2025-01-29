/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.taxonomy.propagation;

import static innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import innowake.mining.shared.entities.*;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagation;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagationJob;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagationModuleIdentificationJob;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagationRequest;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.service.TaxonomyModelService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.TaxonomyReport;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;

/**
 * Performance Test for {@link TaxonomyPropagation}.
 */
@Disabled("Long running performance tests for manual execution")
@WithMockUser
class TaxonomyPropagationPerformanceTest extends DatabaseResettingTest {

	private static final List<RelationshipType> CALLS = List.of(RelationshipType.CALLS);
	private static final Logger LOG = LoggerFactory.getLogger(TaxonomyPropagationPerformanceTest.class);

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private TaxonomyService taxonomyService;
	
	@Autowired
	private TaxonomyModelService taxonomyModelService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	private TaxonomyTypePojo type;

	private EntityId projectId;
	
	String jobId = StringUtils.EMPTY;

	@BeforeEach
	public void insertTestData() {
		projectId = createProject();
		
		final List<TaxonomyCategoryPojo> taxonomy = taxonomyService.findCategories(q-> q.ofProject(projectId).withName("Business Taxonomies"));
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype()
			.setName("DataDomain")
			.setProject(projectId)
			.setCategoryId(taxonomy.get(0).getId())
		);
		type = taxonomyService.getType(typeId);
	}
	
	@Test
	void testTaxonomyPropagationPerformance() {
		/*
		 * 2023-04-25 13:18:20.872 [,,] [main] INFO  innowake.mining.extensions.taxonomy.propagation.TaxonomyPropagationPerformanceTest - Identifying and
		 * returning all the Modules that are going to participate in Taxonomy Propagation involving 10000 modules took 00:00:05.487 and Running Taxonomy
		 * Assignment job involving 10000 modules took 00:00:43.021
		 */
		final List<String> taxonomyIds = new ArrayList<>();
		final List<String> startModuleIds = new ArrayList<>();
		final Map<EntityId, EntityId> moduleTaxonomyMap = new HashMap<>();
		LOG.info("Creating Data for Taxonomy Propagation job");
		final EntityId taxonomy = createTaxonomy(type, "Taxonomy 1");
		for (int i = 0; i < 100; i++) {
			final EntityId moduleI = createModule(projectId, "MODULE I" + i, Technology.COBOL, Type.PROGRAM, Storage.FILE,
				Identification.IDENTIFIED, Origin.CUSTOM);
			taxonomyIds.add(String.valueOf(taxonomy.getUid()));
			startModuleIds.add(String.valueOf(moduleI.getUid()));
			taxonomyService.createModuleLink(moduleI.getUid(), taxonomy);
			for (int j = 0; j < 100; j++) {
				final EntityId moduleJ = createModule(projectId, "MODULE I" + i + " J" + j, Technology.COBOL, Type.PROGRAM, Storage.FILE,
					Identification.IDENTIFIED, Origin.CUSTOM);
				createReference(RelationshipType.CALLS, moduleI, moduleJ);
				moduleTaxonomyMap.put(moduleJ, taxonomy);
			}
			
		}
		LOG.info("Data created for Taxonomy Propagation job");
		
		time(() -> {
			jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(startModuleIds, taxonomyIds, CALLS, CALLS)));
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, "Identifying and returning all the Modules that are going to participate in taxonomy Propagtaion involving 10000 modules");

		time(() -> {
			submitJobFortaxonomyPropagation(new TaxonomyPropagationJob(projectId, getPropagationData(jobId)));
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, "Running Taxonomy Assignment job involving 10000 modules");
		
		final List<EntityId> moduleIds = new ArrayList<>();
		moduleIds.addAll(moduleTaxonomyMap.keySet());
		final List<TaxonomyReport> reportList = taxonomyModelService.findReports(q -> q.ofProject(projectId).ofModules(moduleIds));
		reportList.forEach(report ->
			assertEquals(report.getTaxonomies().get(0).identity(), moduleTaxonomyMap.get(report.getModule().identity())));
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
	
	private EntityId createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))))
				.identity();
	}
	
	private EntityId createModule(final EntityId projectId, final String moduleName, final Technology technology, final Type type, final Storage storage,
			final Identification identification, final Origin origin) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setCreator(Creator.API);
		module.setName(moduleName);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(identification);
		module.setOrigin(origin);
		module.setContent("Test");
		return moduleService.create(module);
	}
	
	private EntityId createTaxonomy(final TaxonomyTypePojo taxonomyType, final String taxonomyName) {
		TaxonomyPojoPrototype proto = new TaxonomyPojoPrototype().setName(taxonomyName).setProject(projectId).setType(taxonomyType.getId());
		return taxonomyService.create(proto);
	}

	private UUID createReference(final RelationshipType relationship, final EntityId incomingIndex, final EntityId outgoingIndex) {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(incomingIndex)
				.setDstModule(outgoingIndex);

		return moduleService.createRelationship(reference);
	}
	
	private Map<String, List<String>> createParameters(final List<String> moduleIds, final List<String> taxonomyIds,
			final List<RelationshipType> incomingReferences, final List<RelationshipType> outgoingReferences) {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleIds", moduleIds);
		parameters.put("taxonomyIds", taxonomyIds);
		parameters.put("incomingReferences", incomingReferences.stream().map(RelationshipType::name).collect(Collectors.toList()));
		parameters.put("outgoingReferences", outgoingReferences.stream().map(RelationshipType::name).collect(Collectors.toList()));
		return parameters;
	}
	
	private String submitJobFortaxonomyPropagation(final Job<?> job) {
		return submitJob(jobManager, tracer, job);
	}
	
	private TaxonomyPropagationModuleIdentificationJob taxonomyPropagationModuleIdentificationJob(final Map<String, List<String>> parameters) {
		final List<EntityId> taxonomyIds = Optional.ofNullable(parameters.get("taxonomyIds"))
													.map(list -> list.stream()
													.map(EntityId::of).collect(Collectors.toList()))
													.orElse(Collections.emptyList());
		final List<RelationshipType> incomings = Optional.ofNullable(parameters.get("incomingReferences"))
													.map(list -> list.stream().map(RelationshipType::from).collect(Collectors.toList()))
													.orElse(Collections.emptyList());
		final List<RelationshipType> outgoings = Optional.ofNullable(parameters.get("outgoingReferences"))
													.map(list -> list.stream().map(RelationshipType::from).collect(Collectors.toList()))
													.orElse(Collections.emptyList());
		final List<DatabaseAccessType> accesses = Optional.ofNullable(parameters.get("readWriteAccess"))
													.map(list -> list.stream().map(DatabaseAccessType::valueOf).collect(Collectors.toList()))
													.orElse(Collections.emptyList());
		final List<EntityId> moduleIds = parameters.get("moduleIds").stream().map(EntityId::of).collect(Collectors.toList());
		final TaxonomyPropagationRequest taxonomyPropagationRequest = new TaxonomyPropagationRequest(moduleIds, taxonomyIds, incomings, outgoings, accesses);
		return new TaxonomyPropagationModuleIdentificationJob(projectId, taxonomyPropagationRequest);
	}
	
	@SuppressWarnings("unchecked")
	private List<PropagationData> getPropagationData(final String jobId) {
		final Result<Serializable> result = (Result<Serializable>) jobManager.getJobResult(jobId);
		assertNotNull("Job result must not be null", result);
		final List<PropagationData> propagationData = (List<PropagationData>) result.value;
		assertNotNull("propagationData must not be null", propagationData);
		return propagationData;
	}

}
