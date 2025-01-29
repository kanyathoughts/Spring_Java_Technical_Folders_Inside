/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.reference.ReferenceServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipType;

/**
 * Command line runner to test reference services.
 */
public class ReferenceExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "c8960c7a-6df5-4f60-8f18-249268d9a581");
		final ReferenceServiceProvider service = MiningApiClient.referenceService(connectionInfo);

		LOG.info("find all Calls references for module id 2000" + MARK);
		Result<ModuleRelationshipPojo[]> result = service.findAllReferencesForModule()
				.setProjectId(EntityId.of(1l))
				.setModuleId(EntityId.of(2000l))
				.setRelationship(RelationshipType.CALLS)
				.execute();
		LOG.info("Status message: " + result.getStatusMessage());
		LOG.info("Result: " + result.getValue().get().length);
		
		LOG.info("find all Calls references" + MARK);
		Result<ModuleRelationshipPojo[]> result2 = service.findAllReferences()
				.setProjectId(EntityId.of(1l))
				.setRelationship(RelationshipType.CALLS)
				.execute();
		LOG.info("Status message: " + result2.getStatusMessage());
		LOG.info("Result: " + result2.getValue().get().length);
		
		LOG.info("find all Calls references in wrong project" + MARK);
		result2 = service.findAllReferences()
				.setProjectId(EntityId.of(2l))
				.setRelationship(RelationshipType.CALLS)
				.execute();
		LOG.info("Status message: " + result2.getStatusMessage());
		LOG.info("Result: " + result2.getValue().get().length);
	}
}
