/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.controlflow;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for calculating the control flow graph of a specific module of a given project.
 */
public class CalculateControlFlowGraphForModule extends ModuleIdService<CalculateControlFlowGraphForModule, String> {
	
	/**
	 * ENDPOINT. the server end point to calculate the Control Flow Graph for a given module.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/control-flow/%s" ;
	private boolean recalculateAst;
	
	/**
	 * Constructor
	 * @param connectionInfo the connection info to use
	 */
	public CalculateControlFlowGraphForModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Calculates Control Flow by sending a HTTP POST request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding JobId { @linkplain Array<char> } if the call was successful
	 */
	@Override
	public Result<String> execute() throws IOException {
		validate();
		setServiceUrl(createUrl());
		return execute(httpPost(), new TypeReference<String>() {});
	}

	/**
	 * Gets the recalculate AST flag.
	 *
	 * @return recalculate AST flag
	 */
	public boolean isRecalculateAst() {
		return recalculateAst;
	}

	/**
	 * Sets the recalculate AST flag.
	 *
	 * @param recalculateAst flag to recalculate AST
	 * @return CalculateControlFlowGraphForModule
	 */
	public CalculateControlFlowGraphForModule setRecalculateAst(boolean recalculateAst) {
		this.recalculateAst = recalculateAst;
		return this;
	}
	
	private String createUrl() {
		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		uri.addParameter("recalculateAst", String.valueOf(recalculateAst));
		return uri.toString();
	}
}
