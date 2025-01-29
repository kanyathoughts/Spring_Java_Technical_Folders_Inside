/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.controlflow;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;

/**
 * HTTP REST service for getting the control flow graph of a specific module of a given project.
 */
public class GetControlFlowGraphForModule extends ModuleIdService<GetControlFlowGraphForModule, ControlFlowGraph> {
	
	/**
	 * ENDPOINT. the server end point to get Control Flow Graph for a given module.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/control-flow/%s" ;
	
	@Nullable
	private Integer characterLimit;
	
	/**
	 * Constructor
	 * @param connectionInfo the connection info to use
	 */
	public GetControlFlowGraphForModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Gets the value of characterLimit
	 *
	 * @return characterLimit for CFG node label
	 */
	@Nullable
	public Integer getCharacterLimit() {
		return characterLimit;
	}
	
	/**
	 * Sets the value of characterLimit.
	 *
	 * @param characterLimit for CFG node label
	 * @return GetControlFlowGraphForModule
	 */
	public GetControlFlowGraphForModule setCharacterLimit(@Nullable final Integer characterLimit) {
		this.characterLimit = characterLimit;
		return this;
	}

	/**
	 * Gets all Control Flow nodes by sending a HTTP GET request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding the {@linkplain ControlFlowGraph} if the call was successful
	 */
	@Override
	public Result<ControlFlowGraph> execute() throws IOException {
		validate();
		setServiceUrl(createUrl());
		return execute(httpGet(), new TypeReference<ControlFlowGraph>() {});
	}
	
	@Override
	protected void validate() {
		super.validate(); 
		if (characterLimit != null && characterLimit.intValue() < 0) {
			throw new IllegalStateException(String.format("The value of characterLimit must not be negative, was %s", characterLimit));
		}
	}
	
	private String createUrl() {
		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		if (characterLimit != null) {
			uri.addParameter("characterLimit", characterLimit.toString());
		}
		return uri.toString();
	}

}
