/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.metamodel;

import java.io.IOException;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.access.EntityId;

/**
 * HTTP REST service call to trigger updating of the metamodel of a given project or in general.
 */
public class RefreshMetamodel extends RestService<Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/refresh/metamodel";
	
	private Optional<EntityId> projectId = Optional.empty();
	
	protected RefreshMetamodel(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Void> execute() throws IOException {
		setServiceUrl(projectId.isPresent() ? ENDPOINT + "?projectId=" + encode(projectId.get()) : ENDPOINT);
		return execute(httpPost());
	}

	public RefreshMetamodel setProjectId(@Nullable final EntityId projectId) {
		this.projectId = Optional.ofNullable(projectId);
		return this;
	}

}
