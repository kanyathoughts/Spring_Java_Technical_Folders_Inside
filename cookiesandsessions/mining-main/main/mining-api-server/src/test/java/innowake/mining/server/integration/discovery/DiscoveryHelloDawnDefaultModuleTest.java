/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.file.Paths;
import java.util.Set;

import org.junit.jupiter.api.AfterAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.TestPropertySource;

import com.google.common.collect.ImmutableSet;

import innowake.mining.server.discovery.dawn.metrics.test.contributors.hello.HelloContributorsConfiguration;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * End-to-end tests for Dawn Default Module discovery using default module contributor implementations.
 */
@TestPropertySource(properties = "configuration.discovery-enable-dawn-contributors=false") /* disable auto-registration of Dawn contributors ... */
@TestPropertySource(properties = "configuration.discovery-enable-legacy-contributors=false") /* disable execution of legacy contributors ... */
@Import(HelloContributorsConfiguration.class) /* ... and only load the "hello" contributors instead */
public class DiscoveryHelloDawnDefaultModuleTest extends DiscoveryBulkTest {

	@Autowired
	private ModuleService moduleService;

	Long moduleId = Long.valueOf(0);

	@Override
	protected String folder() {
		return "dawn-default-modules";
	}

	@Override
	protected BaseDiscoveryTest createTestInstance(final String testFolder, final boolean skipDiscoveryFeatureValidation) {
		return new BaseDiscoveryTest(skipDiscoveryFeatureValidation) {

			@Override
			protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
				createModule(projectId);
				return new DiscoverMetricsJob(projectId, true);
			}

			private void createModule(final EntityId projectId) {
				final ModulePojoPrototype module = new ModulePojoPrototype();
				module.setProject(projectId);
				module.setName("MMRS7101SQLTABLE");
				module.setTechnology(Technology.SQL);
				module.setType(Type.VIEW);
				module.setStorage(Storage.FILE);
				module.setIdentification(Identification.IDENTIFIED);
				module.setOrigin(Origin.CUSTOM);
				module.setCreator(Creator.API);
				moduleId = moduleService.create(module).getNid();
			}

			@Override
			protected String getTestFolder() {
				return Paths.get(folder()).resolve(testFolder).toString();
			}
		};
	}

	@Override
	@AfterAll
	public void after() {
		super.after();
		/* Checks that the module is updated when orCreateIfMissing is set but the target is already available hence it won't create but just update the module
		 * Since MMRS7101SQLTABLE is created even before the discovery it's creator is set to API since the same module is set in orCreateIfMissing it will
		 * just update and won't create again which could be validated by checking that the metrics date has been updated from null value
		 */
		final ModulePojo resultModule = moduleService.getModule(EntityId.of(moduleId));
		assertNotNull(resultModule);
		assertNotNull(resultModule.getMetricsDate());
		/* This confirms that the module type is not updated for the existing module by anchor orCreateIfMissing, 
		 * Since we are identifying the module with the name it remains same */
		assertEquals(ModuleType.SQL_VIEW.getType(), resultModule.getType());
	}

	@Override
	protected Set<String> skipDiscoveryFeatureValidation() {
		/* Will be addressed as part of WMIN-7331 */
		return ImmutableSet.of("sql-modules");
	}
}
