/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.module.DeleteModule;
import innowake.mining.client.service.module.FindAllModules;
import innowake.mining.client.service.module.FindModuleByPath;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Command line runner for testing module services.
 */
public class ModuleExample {

	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);

	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "");
		final ModuleServiceProvider service = MiningApiClient.moduleService(connectionInfo);

		/* find all modules */
		final FindAllModules allModulesService = service.findAllModules();
		LOG.info("Call " + allModulesService.getClass().getSimpleName() + MARK);
		final Result<ModulePojo[]> result = allModulesService.setProjectId(EntityId.of(1l)).execute();
		LOG.info(allModulesService.getClass().getSimpleName() + " status message: " + result.getStatusMessage());
		final ModulePojo[] values = result.getValue().orElse(null);
		final ModulePojo module = assertNotNull(values)[0];

		final FindModuleByPath findModuleByPath = service.findModuleByPath();
		LOG.info("Call " + findModuleByPath.getClass().getSimpleName() + MARK);
		LOG.info(findModuleByPath.setProjectId(EntityId.of(1l)).setPath(module.getPath().orElseThrow()).execute().getStatusMessage());
		
		final DeleteModule deleteModulesService = service.deleteModule();
		LOG.info("Call " + deleteModulesService.getClass().getSimpleName() + MARK);
		LOG.info(deleteModulesService.setProjectId(EntityId.of(1l)).setModuleId(assertNotNull(values)[0].identity()).execute().getStatusMessage());
	}

}
