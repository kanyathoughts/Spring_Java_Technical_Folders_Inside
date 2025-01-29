/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import innowake.mining.server.discovery.dawn.metrics.api.service.DiscoveryService;
import innowake.mining.server.discovery.dawn.metrics.api.service.OngoingDiscovery;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleParameters;

import java.util.List;

public class DummyDiscoveryService implements DiscoveryService {
	@Override
	public OngoingDiscovery discoverMetrics(final TaskHandler taskHandler, final EntityId projectId, final ModuleParameters moduleParameters, final List<EntityId> sourceObjectIds) {
		return new OngoingDiscovery() {
			@Override
			public boolean hasNextCycle() {
				return false;
			}

			@Override
			public void executeNextCycle() {
				/* does nothing */
			}
		};
	}
}
