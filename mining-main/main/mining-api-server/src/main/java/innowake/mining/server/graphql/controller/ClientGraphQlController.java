/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.stereotype.Controller;

import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;

/**
 * Controller for the "client" GraphQl Query.
 */
@Controller
public class ClientGraphQlController implements MiningDataPointSource {
	
	@Autowired
	private ClientService clientService;
	
	@MiningQueryMapping
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role({VIEWER})
	public ClientPojo client(@Argument final Long clientId) {
		return clientService.get(EntityId.of(clientId), true);
	}

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineType("Client")
			.representedBy(ClientPojo.class)
			.withDefaultProperties()
			.add();
		builder.defineDataPointsFromSchemaMappingAnnotations(this);
	}
}
