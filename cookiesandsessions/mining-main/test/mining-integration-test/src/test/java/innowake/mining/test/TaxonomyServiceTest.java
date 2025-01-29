/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.client.service.taxonomy.CreateTaxonomy;
import innowake.mining.client.service.taxonomy.TaxonomyServiceProvider;
import innowake.mining.client.service.taxonomytype.TaxonomyTypeServiceProvider;
import innowake.mining.data.access.postgres.TaxonomyPgDao;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.TaxonomyReport;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsGetRequest;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsGetResponse;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;
import innowake.mining.test.util.JobStatusUtil;
import org.hamcrest.Matchers;
import org.hamcrest.core.IsNot;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * Integration tests for the {@link TaxonomyPojo} service.
 */
public class TaxonomyServiceTest extends IntegrationTest {

	private static final String TEST_NAME_1 = "test 1";
	private static final EntityId ONE = EntityId.of(Long.valueOf(1));
	private static final Long THREE = Long.valueOf(3);
	private static final EntityId NON_EXISTING_ID = EntityId.of(Long.valueOf(Long.MAX_VALUE));
	private static final TaxonomyTypePojoPrototype DATA_DOMAIN = new TaxonomyTypePojoPrototype();
	private static final TaxonomyTypePojoPrototype BUSINESS_PROCESS = new TaxonomyTypePojoPrototype();
	private static final TaxonomyTypePojoPrototype BUSINESS_SUBSYSTEM = new TaxonomyTypePojoPrototype();
	private static final TaxonomyTypePojoPrototype NON_EXISTING_TAXONOMY_TYPE = new TaxonomyTypePojoPrototype();
	private final String IDENTIFIED_MODULE_MSG = "Identified 1 Module(s) in selection which are supported by the Technical Taxonomy identification";

	private final TaxonomyServiceProvider taxonomyServiceProvider = MiningApiClient.taxonomyService(getConnectionInfo());
	private final TaxonomyTypeServiceProvider taxonomyTypeServiceProvider = MiningApiClient.taxonomyTypeService(getConnectionInfo());
	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	private final JobServiceProvider jobServiceProvider = MiningApiClient.jobService(getConnectionInfo());
	
	@BeforeAll
	public static void initTypes() {
		DATA_DOMAIN.setName("DataDomain");
		DATA_DOMAIN.setProject(ONE);
		BUSINESS_PROCESS.setName("BusinessProcess");
		BUSINESS_PROCESS.setProject(ONE);
		BUSINESS_SUBSYSTEM.setName("BusinessSubsystem");
		BUSINESS_SUBSYSTEM.setProject(ONE);
		NON_EXISTING_TAXONOMY_TYPE.setName("I DO NOT EXIST ... BUT THAT'S OKAY");
		NON_EXISTING_TAXONOMY_TYPE.setProject(ONE);
	}
	
	@Test
	void createTaxonomyTest() throws IOException {
		final TaxonomyPojoPrototype dataDomainTaxonomyExpected = getTaxonomyTestObject(DATA_DOMAIN);
		final TaxonomyPojo dataDomainTaxonomyActual = createTaxonomy(dataDomainTaxonomyExpected);
		verifyTaxonomyWithoutId(dataDomainTaxonomyExpected, dataDomainTaxonomyActual);
		final Object property = CustomProperties.getCustomPropertyByName("customTaxonomyProperty", dataDomainTaxonomyActual);
		assertEquals("A custom created property value", property);
		
		final TaxonomyPojoPrototype busionessProcessTaxonomyExpected = getTaxonomyTestObject(BUSINESS_PROCESS);
		final TaxonomyPojo busionessProcessTaxonomyActual = createTaxonomy(busionessProcessTaxonomyExpected);
		verifyTaxonomyWithoutId(busionessProcessTaxonomyExpected, busionessProcessTaxonomyActual);
		
		final TaxonomyPojoPrototype BusinessSubsystemTaxonomyExpected = getTaxonomyTestObject(BUSINESS_SUBSYSTEM);
		final TaxonomyPojo BusinessSubsystemTaxonomyActual = createTaxonomy(BusinessSubsystemTaxonomyExpected);
		verifyTaxonomyWithoutId(BusinessSubsystemTaxonomyExpected, BusinessSubsystemTaxonomyActual);
	}
	
	@Test
	void createTaxonomyAlreadyExistsTest() throws IOException {
		final TaxonomyPojoPrototype dataDomainTaxonomy = getTaxonomyTestObject(DATA_DOMAIN);
		createTaxonomy(dataDomainTaxonomy);
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(ONE).setTaxonomy(dataDomainTaxonomy).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void createTaxonomyWithoutProjectUrl() throws IOException {
		final CreateTaxonomy dataDomainTaxonomy = taxonomyServiceProvider.createTaxonomy().setTaxonomy(getTaxonomyTestObject(DATA_DOMAIN));
		Assertions.assertThrows(IllegalStateException.class, dataDomainTaxonomy::execute);
	}
	
	@Test
	void createTaxonomyWithoutNameTest() throws IOException {
		final Optional<UUID> type = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
				.setProjectId(ONE)
				.execute().getValue().map(Arrays::asList)
				.get().stream()
				.filter(q -> DATA_DOMAIN.name.getNonNull().equals(q.getName()))
				.map(q -> q.getId())
				.findAny();
			
		
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setProject(ONE)
				.setType(type.get());
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(ONE).setTaxonomy(taxonomy).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void createTaxonomyWithNonExistingTaxonomyType() throws IOException {
		final TaxonomyPojoPrototype taxonomy = getTaxonomyTestObject(NON_EXISTING_TAXONOMY_TYPE);
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(ONE).setTaxonomy(taxonomy).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void createTaxonomyWithoutProjectTest() throws IOException {
		final Optional<UUID> type = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
				.setProjectId(ONE)
				.execute().getValue().map(Arrays::asList)
				.get().stream()
				.filter(q -> DATA_DOMAIN.name.getNonNull().equals(q.getName()))
				.map(q -> q.getId())
				.findAny();
		
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName(TEST_NAME_1)
				.setType(type.get());
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(ONE).setTaxonomy(taxonomy).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
	}
	
	@Test
	void createTaxonomyWithoutTypeTest() throws IOException {
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName(TEST_NAME_1)
				.setProject(ONE);
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(ONE).setTaxonomy(taxonomy).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void createTaxonomyWithoutExistingProjectTest() throws IOException {
		final Optional<UUID> type = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
				.setProjectId(ONE)
				.execute().getValue().map(Arrays::asList)
				.get().stream()
				.filter(q -> DATA_DOMAIN.name.getNonNull().equals(q.getName()))
				.map(q -> q.getId())
				.findAny();
		
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName(TEST_NAME_1)
				.setProject(NON_EXISTING_ID)
				.setType(type.get());
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(NON_EXISTING_ID).setTaxonomy(taxonomy).execute();
		assertEquals(404, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void deleteTaxonomyNonExistingTest() throws IOException {
		final Result<Void> result = taxonomyServiceProvider.deleteTaxonomy().setProjectId(ONE).setId(NON_EXISTING_ID).execute();
		assertEquals(404, result.getStatusCode());
	}
	
	@Test
	void deleteAndFindAllTaxonomyTest() throws IOException {
		/* create a taxonomy with the same name for each type*/
		final TaxonomyPojoPrototype taxonomyToDeleteProto = getTaxonomyTestObject(BUSINESS_PROCESS);
		final TaxonomyPojo taxonomyToDelete = createTaxonomy(taxonomyToDeleteProto);
		final EntityId taxonomyToDeleteId = taxonomyToDelete.identity();
		final TaxonomyPojoPrototype budinessSubsystemTaxonomyNotToDeleteProto = getTaxonomyTestObject(BUSINESS_SUBSYSTEM);
		final var businessSubsystemTaxonomyNotToDelete = createTaxonomy(budinessSubsystemTaxonomyNotToDeleteProto);
		final TaxonomyPojo dataDomainTaxonomyNotToDelete = createTaxonomy(getTaxonomyTestObject(DATA_DOMAIN));

		/* verify that the element that should be deleted is actually present */
		final Result<TaxonomyPojo[]> resultFindBeforeDelete = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).setTaxonomyType(BUSINESS_PROCESS.name.getNonNull()).execute();
		assertEquals(200, resultFindBeforeDelete.getStatusCode());
		assertThat(Arrays.asList(resultFindBeforeDelete.getValue().get()), Matchers.hasItem(taxonomyToDelete));

		/* delete the new taxonomy for the type BUSINESS_PROCESS */
		final Result<Void> result = taxonomyServiceProvider.deleteTaxonomy().setProjectId(ONE).setId(taxonomyToDeleteId).execute();
		assertEquals(204, result.getStatusCode());
		
		/* verify that the element was deleted and that the other two elements are still present */
		final Result<TaxonomyPojo[]> resultFindAfterDelete = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).execute();
		assertEquals(200, resultFindAfterDelete.getStatusCode());
		final TaxonomyPojo[] allTaxonomies = resultFindAfterDelete.getValue().get();
		assertFalse(Arrays.asList(allTaxonomies).contains(taxonomyToDelete));
		assertThat(Arrays.asList(allTaxonomies), Matchers.hasItem(businessSubsystemTaxonomyNotToDelete));
		assertThat(Arrays.asList(allTaxonomies), Matchers.hasItem(dataDomainTaxonomyNotToDelete));
	}
	
	@Test
	void updateAndFindAllTaxonomyTest() throws IOException {
		/* create a taxonomy with the same name for each type*/
		final TaxonomyPojoPrototype taxonomyInitialProto = getTaxonomyTestObject(BUSINESS_PROCESS);
		final TaxonomyPojo taxonomyInitial = createTaxonomy(taxonomyInitialProto);
		final TaxonomyPojoPrototype taxonomyToUpdate = new TaxonomyPojoPrototype()
				.setName("UPDATED TAXONOMY")
				.setCustomProperties(new NestedMap().set(CustomPropertyClass.TaxonomyCustomProperties.name(), "customTaxonomyProperty", "An updated value for the property"));
		final TaxonomyPojo budinessSubsystemTaxonomyNotToUpdate = createTaxonomy(getTaxonomyTestObject(BUSINESS_SUBSYSTEM));
		final TaxonomyPojo dataDomainTaxonomyNotToUpdate = createTaxonomy(getTaxonomyTestObject(DATA_DOMAIN));

		/* verify that the element that should be updated is actually present */
		final Result<TaxonomyPojo[]> resultFindBeforeUpdate = taxonomyServiceProvider.findAllTaxonomies()
				.setProjectId(ONE).setTaxonomyType(BUSINESS_PROCESS.name.getNonNull()).execute();
		assertEquals(200, resultFindBeforeUpdate.getStatusCode());
		assertThat(Arrays.asList(resultFindBeforeUpdate.getValue().get()), Matchers.hasItem(taxonomyInitial));

		/* update the new taxonomy for the type BUSINESS_PROCESS */
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.updateTaxonomy()
				.setProjectId(ONE).setId(taxonomyInitial.identity()).setTaxonomy(taxonomyToUpdate).execute();
		assertEquals(result.getExtendedStatusMessage(), 200, result.getStatusCode());
		
		/* verify that the element was updated and that the other two elements are still unchanged */
		final TaxonomyPojo updatedTaxonomy = result.getValue().get();
		assertEquals(ONE, updatedTaxonomy.getProject());
		assertEquals(BUSINESS_PROCESS.name.getNonNull(), updatedTaxonomy.getType().getName());
		assertEquals("UPDATED TAXONOMY", updatedTaxonomy.getName());
		assertEquals("An updated value for the property", updatedTaxonomy.getCustomProperties()
				.getValue(CustomPropertyClass.TaxonomyCustomProperties.name(), "customTaxonomyProperty"));

		final Result<TaxonomyPojo[]> resultFindAfterUpdate = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).execute();
		assertEquals(200, resultFindAfterUpdate.getStatusCode());
		final TaxonomyPojo[] allTaxonomies = resultFindAfterUpdate.getValue().get();

		assertThat(Arrays.asList(allTaxonomies), IsNot.not(Matchers.hasItem(taxonomyInitial)));
		assertThat(Arrays.asList(allTaxonomies), Matchers.hasItem(budinessSubsystemTaxonomyNotToUpdate));
		assertThat(Arrays.asList(allTaxonomies), Matchers.hasItem(dataDomainTaxonomyNotToUpdate));
	}

	@Test
	void updateNonExistingTaxonomy() throws IOException {
		final TaxonomyPojoPrototype taxonomyToUpdate = getTaxonomyTestObject(BUSINESS_PROCESS);
		taxonomyToUpdate.withId(NON_EXISTING_ID);
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.updateTaxonomy()
				.setProjectId(ONE).setId(taxonomyToUpdate.identityProvisional()).setTaxonomy(taxonomyToUpdate).execute();
		assertEquals(404, result.getStatusCode());
	}

	@Test
	void updateWithoutId() throws IOException {
		final TaxonomyPojoPrototype taxonomyToUpdate = getTaxonomyTestObject(BUSINESS_PROCESS);
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.updateTaxonomy()
				.setProjectId(ONE).setId(EntityId.of(Long.MAX_VALUE)).setTaxonomy(taxonomyToUpdate).execute();
		assertEquals(404, result.getStatusCode());
	}

	@Test
	void updateToDifferentTypeTest() throws IOException {
		final TaxonomyPojoPrototype taxonomyProto = getTaxonomyTestObject(BUSINESS_PROCESS);
		final TaxonomyPojo taxonomyToUpdate = createTaxonomy(taxonomyProto);
		
		final Optional<UUID> type = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
				.setProjectId(ONE)
				.execute().getValue().map(Arrays::asList)
				.get().stream()
				.filter(q -> DATA_DOMAIN.name.getNonNull().equals(q.getName()))
				.map(q -> q.getId())
				.findAny();

		taxonomyProto.setType(type.get());
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.updateTaxonomy()
				.setProjectId(ONE).setId(taxonomyToUpdate.identity()).setTaxonomy(taxonomyProto).execute();
		assertEquals(result.getExtendedStatusMessage(), 400, result.getStatusCode());
	}
	
	@Test
	void findAllTaxonomiesWithTypeJdbcTest() throws IOException {
		/* create a taxonomy with the same name for each type to ensure that each has at least one element */
		createTaxonomy(getTaxonomyTestObject(BUSINESS_PROCESS));
		createTaxonomy(getTaxonomyTestObject(BUSINESS_SUBSYSTEM));
		var dataDomain = createTaxonomy(getTaxonomyTestObject(DATA_DOMAIN));
		
		final Result<TaxonomyPojo[]> result = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).setTaxonomyType(DATA_DOMAIN.name.getNonNull()).execute();
		assertEquals(200, result.getStatusCode());
		final TaxonomyPojo[] taxonomies = result.getValue().get();
		verifyWithJdbc(taxonomies, dataDomain.getType().getId());
		
		for (final TaxonomyPojo taxonomy : taxonomies) {
			if (taxonomy.getName().equals("Employee domain")) {
				assertEquals(1, taxonomy.getCustomProperties().size());
				final Object property = CustomProperties.getCustomPropertyByName("customTaxonomyProperty", taxonomy);
				assertEquals("A value for the custom Taxonomy property", property);
			}
		}
	}
	
	@Test
	void countOnlyModulesWithCertainTaxonomy() throws IOException {
		final Result<TaxonomyPojo[]> result = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).execute();
		final Result<TaxonomyPojo[]> resultWithTxId = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).setCountOnlyModulesWithTaxonomyId(THREE).execute();
		assertEquals(200, result.getStatusCode());
		assertEquals(200, resultWithTxId.getStatusCode());
		final Optional<TaxonomyPojo> taxonomy = asList(result.getValue().get()).stream().filter(txnmy -> txnmy.getName().equals("Employee domain")).findFirst();
		final Optional<TaxonomyPojo> taxonomyWithTxId = asList(resultWithTxId.getValue().get()).stream().filter(txnmy -> txnmy.getName().equals("Employee domain")).findFirst();

		assertNotEquals(taxonomy.get().getTaxonomyReferenceCount(), taxonomyWithTxId.get().getTaxonomyReferenceCount());
	}
	
	@Test
	void findAllTaxonomiesWithNonExistingTypeJdbcTest() throws IOException {
		/* create a taxonomy with the same name for each type to ensure that each has at least one element */
		createTaxonomy(getTaxonomyTestObject(BUSINESS_PROCESS));
		createTaxonomy(getTaxonomyTestObject(BUSINESS_SUBSYSTEM));
		createTaxonomy(getTaxonomyTestObject(DATA_DOMAIN));
		
		final Result<TaxonomyPojo[]> result = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).setTaxonomyType(NON_EXISTING_TAXONOMY_TYPE.name.getNonNull()).execute();
		assertEquals(200, result.getStatusCode());
		final TaxonomyPojo[] taxonomies = result.getValue().get();
		assertEquals(0, taxonomies.length);
	}

	@Test
	void findAllTaxonomiesJdbcTest() throws IOException {
		/* create a taxonomy with the same name for each type to ensure that each has at least one element */
		createTaxonomy(getTaxonomyTestObject(BUSINESS_PROCESS));
		createTaxonomy(getTaxonomyTestObject(BUSINESS_SUBSYSTEM));
		createTaxonomy(getTaxonomyTestObject(DATA_DOMAIN));
		
		final Result<TaxonomyPojo[]> result = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).execute();
		assertEquals(200, result.getStatusCode());
		final TaxonomyPojo[] taxonomies = result.getValue().get();
		verifyWithJdbc(taxonomies, null);
	}

	private void verifyWithJdbc(final TaxonomyPojo[] taxonomies, @Nullable final UUID typeId) {
		final var actual = EntityId.allUids(new TaxonomyPgDao(getDataSource()).findIds(q -> {
			q.ofProject(ONE);
			if (typeId != null) {
				q.ofType(typeId);
			}
		}));
		
		final List<UUID> expected = Stream.of(taxonomies).map(pojo -> pojo.getUid()).collect(Collectors.toList());
		
		assertTrue(actual.containsAll(expected));
	}

	@Test
	void findAllTaxonomiesNonExistingProject() throws IOException {
		final Result<TaxonomyPojo[]> result = taxonomyServiceProvider.findAllTaxonomies().setProjectId(NON_EXISTING_ID).execute();
		assertEquals(404, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	/**
	 * Tests that the REST service invocation for the Technical TaxonomyPojo identification properly triggers a job that successfully executes.
	 * 
	 * @throws IOException if the REST calls cannot be executed
	 */
	@Test
	void technicalTaxonomyIdentification() throws IOException {
		/* submit job */
		final List<String> modulePaths = new ArrayList<>();
		modulePaths.add("src/cobol/programs/EXECSQL.cbl");
		modulePaths.add("src-natural/LibA/PRG1.nsp");
		modulePaths.add("src/cobol/programs/CC1.cpy");
		final Result<String> result = taxonomyServiceProvider.identifyTechnicalTaxonomies().setProjectId(ONE).setModulePaths(modulePaths).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		
		/* wait for the job to finish */
		final JobInformation jobInfo = waitForJob(jobId);
		
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		/* There are 2 messages, one complaining about missing source and one containing the number of supported Modules in the path selection */
		final List<Message> messages = jobInfo.getMessages();
		assertEquals(3, messages.size());
		assertThat(messages.get(1).getText(), containsString("2 Module(s)"));
		assertThat(messages.get(2).getText(), containsString("2 module(s) were successful."));
	}
	
	/**
	 * Tests that the REST service invocation for the Technical TaxonomyPojo identification properly triggers a job that successfully executes.
	 * 
	 * @throws IOException if the REST calls cannot be executed
	 */
	@Test
	void technicalTaxonomyIdentificationforBasic() throws IOException {
		/* submit job */
		final List<String> modulePaths = new ArrayList<>();
		modulePaths.add("src/cobol/programs/BASICO.bas");
		final Result<String> result = taxonomyServiceProvider.identifyTechnicalTaxonomies().setProjectId(ONE).setModulePaths(modulePaths).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		
		/* wait for the job to finish */
		final JobInformation jobInfo = waitForJob(jobId);
		
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		/* There is only 1 message containing the number of supported Modules in the path selection */
		final List<Message> messages = jobInfo.getMessages();
		assertEquals(3, messages.size());
		assertTrue(messages.stream().map(message -> message.getText()).anyMatch(s -> s.contains("3 Module(s)")));
		assertThat(messages.get(2).getText(), containsString("3 module(s) were successful."));
	}
	
	/**
	 * Tests that the REST service invocation for the Technical TaxonomyPojo identification properly triggers a job that successfully executes.
	 * 
	 * @throws IOException if the REST calls cannot be executed
	 */
	@Test
	void technicalTaxonomyIdentificationforNatural() throws IOException {
		/* submit job */
		final List<String> modulePaths = new ArrayList<>();
		modulePaths.add("src/natural/programs/NATPRGA.nsp");
		modulePaths.add("src/natural/programs/NATCCA.nsc");
		final Result<String> result = taxonomyServiceProvider.identifyTechnicalTaxonomies().setProjectId(ONE).setModulePaths(modulePaths).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		final JobInformation jobInfo = waitForJob(jobId);

		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		final List<Message> messages = jobInfo.getMessages();
		assertEquals(2, messages.size());
		assertThat(messages.get(0).getText(), containsString("1 Module"));
		assertThat(messages.get(1).getText(), containsString("1 module(s) were successful."));
	}

	/**
	 * Checks the log output {@link JobInformation} of the Technical TaxonomyPojo identification for a module without sourceCode.
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testTechnicalTaxonomyIdentificationWithoutSourceCode() throws IOException {
		final String modulePath = "src/cobol/programs/PRGTEST.cbl";
		/* Submit job */
		final Result<String> moduleInfo = taxonomyServiceProvider.identifyTechnicalTaxonomies().setProjectId(ONE).setModulePaths(asList(modulePath)).execute();
		assertNotNull(moduleInfo);
		assertEquals(202, moduleInfo.getStatusCode());
		assertTrue(moduleInfo.getValue().isPresent());
		final String jobId = moduleInfo.getValue().get();
		final JobInformation jobInformation = JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		final List<Message> messages = jobInformation.getMessages();
		assertEquals(3, messages.size());
		final var textWithoutUid = messages.get(0).getText().replaceAll("uid=[0-9a-z-]+", "uid=someUid");
		assertEquals(String.format(SOURCE_CODE_NOT_FOUND, Integer.valueOf(2037), "PRGTEST"), textWithoutUid);
		assertEquals(IDENTIFIED_MODULE_MSG, messages.get(1).getText());
		assertThat(messages.get(2).getText(), containsString("1 module(s) were successful."));
	}
	
	@Test
	void testTaxonomyReports() throws IOException {
		
		final ModulePojoPrototype MODULE = new ModulePojoPrototype()
				.setId("2000")
				.setName("PRG1")
				.setProject(ONE)
				.setTechnology(Technology.NATURAL)
				.setType(Type.PROGRAM)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setStorage(Storage.FILE);
		final ModulePojo modulePojo = moduleServiceProvider.createModule().setModule(MODULE).setProjectId(ONE).execute().getValue().get();
		
		final Optional<UUID> type = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
				.setProjectId(EntityId.of(1L))
				.execute().getValue().map(Arrays::asList)
				.get().stream()
				.filter(q -> "BusinessSubsystem".equals(q.getName()))
				.map(q -> q.getId())
				.findAny();

		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setId("16")
				.setProject(EntityId.of(1L))
				.setName("ARB100x")
				.setType(type.get());

		final TaxonomyPojo resultTaxonomy = taxonomyServiceProvider.createTaxonomy().setTaxonomy(taxonomy).setProjectId(ONE).execute().getValue().get();
		
		final TaxonomyReport taxonomyReport = new TaxonomyReport(modulePojo, Arrays.asList(resultTaxonomy));
		final Result<TaxonomyReport[]> result = taxonomyServiceProvider.getTaxonomyReport().setProjectId(ONE).execute();
		assertEquals(200, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		assertEquals("PRG1", taxonomyReport.getModule().getName());
		assertEquals(1, taxonomyReport.getTaxonomies().size());
		assertEquals("ARB100x", taxonomyReport.getTaxonomies().get(0).getName());
	}
	
	/**
	 * Tests the get TaxonomyPojo Assignments REST endpoint with modules from multiple projects
	 * 
	 * @throws IOException if REST call was not successful
	 */
	@Test
	void testGetTaxonomyAssignments() throws IOException {
		/* Module IDs 2000, 2001 belongs to Project ID 1 while 2003 does not */
		final ModuleMatcher modules = new ModuleMatcher(
				asList(EntityId.of(2001L)),
				asList(/* 2000 */ "src-natural/LibA/**/*1*", /* 2003 */ "src/cobol/programs/MMRS7102.cbl"));
		final TaxonomyAssignmentsGetRequest request = new TaxonomyAssignmentsGetRequest(modules);
		
		final Result<TaxonomyPojo []> taxonomies = taxonomyServiceProvider.findAllTaxonomies().setProjectId(ONE).execute();
		final Result<TaxonomyAssignmentsGetResponse> taxonomyAssignmentGetResponse =
				taxonomyServiceProvider.getTaxonomyAssignments().setProjectId(ONE).setRequest(request).execute();
		
		assertEquals("Response Status should be OK", 200, taxonomyAssignmentGetResponse.getStatusCode());
		assertEquals("Response Status should be OK", 200, taxonomies.getStatusCode());
		assertTrue("Response body should not be null", taxonomyAssignmentGetResponse.getValue().isPresent());
		assertTrue("Response body should not be null", taxonomies.getValue().isPresent());

		final TaxonomyAssignmentsGetResponse resultValue = taxonomyAssignmentGetResponse.getValue().get();
		assertEquals("Module count should be 2 as only modules belonging to Project 1 are valid", Long.valueOf(2), resultValue.getModuleCount());
		assertEquals("TaxonomyPojo object should be returned for every taxonomy in the project",
				taxonomies.getValue().get().length, resultValue.getTaxonomies().size());
		for (final TaxonomyAssignmentsGetResponse.TaxonomyGetAssignment assignment : resultValue.getTaxonomies()) {
			final TaxonomyPojo taxonomy = taxonomyServiceProvider.findTaxonomyById().setId(assignment.getTaxonomy().identity()).setProjectId(ONE).execute().getValue().get();
			final String taxonomyName = taxonomy.getName();
			final AssignmentState state = assignment.getState();
			if ("Employee domain".equals(taxonomyName)) {
				assertEquals(AssignmentState.ALL, state);
			} else if ("Create Invoices".equals(taxonomyName)) {
				assertEquals(AssignmentState.NONE, state);
			} else if ("ARB100".equals(taxonomyName)) {
				assertEquals(AssignmentState.SOME, state);
			}
		}
	}
	
	/**
	 * Tests the update TaxonomyPojo Assignments REST endpoint for valid assignment
	 *
	 * @throws IOException if REST call was not successful
	 */
	@Test
	void testUpdateTaxonomyAssignmentsWithValidDetails() throws IOException {
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignment = new TaxonomyAssignmentsSetRequest
				.TaxonomySetAssignment(createTaxonomy(getTaxonomyTestObject(BUSINESS_PROCESS)).identity(), AssignmentState.ALL);
		final ModuleMatcher matcher = new ModuleMatcher(
				asList(EntityId.of(2009L), EntityId.of(2003L)), 
				asList(/* 2017 */ "src/cobol/programs/DPGM1.cpy",
						/* 2025 */ "src/cobol/programs/DP*9.cpy"));
		final TaxonomyAssignmentsSetRequest request = new TaxonomyAssignmentsSetRequest(matcher, Collections.singletonList(assignment));
		final Result<Void> result = taxonomyServiceProvider.updateTaxonomyAssignments().setProjectId(ONE).setRequest(request).execute();
		assertEquals(202, result.getStatusCode());
		/* The verification logic only works with IDs, so we need to provide the corresponding IDs instead of paths. */
		verifyTaxonomyAssignments(asList(EntityId.of(2009L), EntityId.of(2017L), EntityId.of(2025L)), TEST_NAME_1, 1);
		/* 2003 is not part of the project and therefore should not have any TaxonomyPojo assigned to */
		verifyTaxonomyAssignments(asList(EntityId.of(2003L)), TEST_NAME_1, 0);
	}
	
	/**
	 * Tests the update TaxonomyPojo Assignments REST endpoint for valid unassignment
	 *
	 * @throws IOException if REST call was not successful
	 */
	@Test
	void testTaxonomyUnassignmentWithValidDetails() throws IOException {
		final List<EntityId> moduleIds = Collections.singletonList(EntityId.of(2000L));
		
		/* Ensure that there is already a TaxonomyPojo assigned to the Module */
		final String taxonomyName = "Employee domain";
		final List<EntityId> taxonomies = verifyTaxonomyAssignments(moduleIds, taxonomyName, 1);

		/* Unassigning that taxonomy */
		final List<TaxonomyAssignmentsSetRequest.TaxonomySetAssignment> assignments = taxonomies.stream()
				.map(id -> new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(id, AssignmentState.NONE))
				.collect(Collectors.toList());
		final TaxonomyAssignmentsSetRequest request = new TaxonomyAssignmentsSetRequest(new ModuleMatcher(moduleIds, emptyList()), assignments);
		final Result<Void> unassignmentResult = taxonomyServiceProvider.updateTaxonomyAssignments().setProjectId(ONE).setRequest(request).execute();
		assertEquals(202, unassignmentResult.getStatusCode());
		verifyTaxonomyAssignments(moduleIds, taxonomyName, 0);
	}
	
	/**
	 * Tests the update TaxonomyPojo Assignments REST endpoint with state not set and state as {@link AssignmentState#SOME}
	 *
	 * @throws IOException if REST call was not successful
	 */
	@Test
	void testUpdateTaxonomyAssignmentsWithStateNotSetAndStateSome() throws IOException {
		/* create a new taxonomy */
		final TaxonomyPojoPrototype taxonomyToAssignProto = getTaxonomyTestObject(BUSINESS_PROCESS);
		final TaxonomyPojo taxonomyToAssign = createTaxonomy(taxonomyToAssignProto);

		final ModuleMatcher matcher = new ModuleMatcher(asList(EntityId.of(2009L), EntityId.of(2017L), EntityId.of(2025L)), emptyList());
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment taxonomy = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyToAssign.identity(), AssignmentState.SOME);
		final TaxonomyAssignmentsSetRequest request = new TaxonomyAssignmentsSetRequest(matcher, Collections.singletonList(taxonomy));
		
		/* try to assign the new taxonomy for the type BUSINESS_PROCESS */
		assertEquals("Trying to assign the new taxonomy with state SOME should result in BAD REQUEST",
				400, taxonomyServiceProvider.updateTaxonomyAssignments().setProjectId(ONE).setRequest(request).execute().getStatusCode());
		
		final TaxonomyAssignmentsGetRequest getRequest = new TaxonomyAssignmentsGetRequest(matcher);
		final Result<TaxonomyAssignmentsGetResponse> getResult = taxonomyServiceProvider.getTaxonomyAssignments().setProjectId(ONE).setRequest(getRequest).execute();
		final List<TaxonomyAssignmentsGetResponse.TaxonomyGetAssignment> taoxnomies = getResult.getValue().get().getTaxonomies();
		/* verify that no assignments were created */
		assertEquals(0, taoxnomies.stream().filter(a -> a.getState() != AssignmentState.NONE).count());
	}
	
	private List<EntityId> verifyTaxonomyAssignments(final List<EntityId> moduleIds, final String taxonomyName, final long expectedAssignments) throws IOException {
		final ModuleMatcher matcher = new ModuleMatcher(moduleIds, emptyList());
		final TaxonomyAssignmentsGetRequest getRequest = new TaxonomyAssignmentsGetRequest(matcher);
		final List<TaxonomyAssignmentsGetResponse.TaxonomyGetAssignment> getResult = taxonomyServiceProvider.getTaxonomyAssignments()
				.setProjectId(ONE).setRequest(getRequest).execute().getValue().get().getTaxonomies();

		final List<EntityId> taxonomies = new ArrayList<>();
		long taoxnomyCount = 0;
		for (final TaxonomyAssignmentsGetResponse.TaxonomyGetAssignment assignment : getResult) {
			final TaxonomyPojo pojo = taxonomyServiceProvider.findTaxonomyById()
					.setProjectId(ONE).setId(assignment.getTaxonomy().identity())
					.execute().getValue().get();
			if (taxonomyName.equals(pojo.getName()) && assignment.getState() != AssignmentState.NONE) {
				taoxnomyCount++;
				taxonomies.add(pojo.identity());
			}
		}
		assertEquals(expectedAssignments, taoxnomyCount);
		return taxonomies;
	}

	private void verifyTaxonomyWithoutId(final TaxonomyPojoPrototype dataDomainTaxonomyExpected, final TaxonomyPojo actual) {
		assertEquals(dataDomainTaxonomyExpected.name.getNonNull(), actual.getName());
		assertEquals(dataDomainTaxonomyExpected.type.getNonNull(), actual.getType().getId());
		assertEquals(dataDomainTaxonomyExpected.project.getNonNull(), actual.getProject());
	}
	
	private TaxonomyPojoPrototype getTaxonomyTestObject(final TaxonomyTypePojoPrototype type) throws IOException {
		final Optional<UUID> typeId = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
				.setProjectId(type.project.getNonNull())
				.execute().getValue().map(Arrays::asList)
				.get().stream()
				.filter(q -> type.name.getNonNull().equals(q.getName()))
				.map(q -> q.getId())
				.findAny();
		
		return new TaxonomyPojoPrototype()
				.setName(TEST_NAME_1)
				.setProject(ONE)
				.setType(typeId.orElseGet(() -> new UUID(0L, 0L)))
				.setCustomProperties(new HashMap<>(Map.of(CustomPropertyClass.TaxonomyCustomProperties.name(),new HashMap<>(Map.of("customTaxonomyProperty", "A custom created property value")))));
	}
	
	private TaxonomyPojo createTaxonomy(final TaxonomyPojoPrototype taxonomy) throws IOException {
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy().setProjectId(ONE).setTaxonomy(taxonomy).execute();
		assertEquals(201, result.getStatusCode());
		final Optional<TaxonomyPojo> resultTaxonomy = result.getValue();
		assertTrue(resultTaxonomy.isPresent());
		return resultTaxonomy.get();
	}
	
	public static HashMap<Long, String> queryTaxonomyCategories(final Long projectId) {
		final HashMap<Long, String> taxonomiesCategories = new HashMap<>();
		final var taxonomyDao = new TaxonomyPgDao(getDataSource());
		taxonomyDao.findCategories(q -> q.ofProject(EntityId.of(projectId)))
			.forEach(t -> taxonomiesCategories.put(t.getId(), t.getName()));
		return taxonomiesCategories;
	}
	
	private JobInformation waitForJob(final String jobId) throws IOException {
		/* wait for the job to finish */
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		while (Duration.between(start, Instant.now()).toMinutes() < 2) {
			final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
			assertNotNull(jobInfoResult);
			assertTrue(jobInfoResult.getValue().isPresent());
			jobInfo = jobInfoResult.getValue().get();
			
			final JobStatus status = jobInfo.getStatus();
			if (status != JobStatus.RUNNING && status != JobStatus.SCHEDULED) {
				break;
			}
		}
		
		final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
		assertNotNull(jobInfoResult);
		assertTrue(jobInfoResult.getValue().isPresent());
		jobInfo = jobInfoResult.getValue().get();
		return jobInfo;
	}
}
