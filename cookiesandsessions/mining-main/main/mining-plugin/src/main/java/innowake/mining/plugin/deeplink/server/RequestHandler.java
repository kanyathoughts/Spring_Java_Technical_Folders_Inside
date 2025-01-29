/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.server;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.plugin.Logging;
import innowake.mining.plugin.deeplink.command.Request;
import innowake.mining.plugin.deeplink.command.RequestExecutionException;
import innowake.mining.plugin.deeplink.command.RequestParseException;
import innowake.mining.plugin.deeplink.command.Executor;

/**
 * This class takes care of incoming requests to the server.
 * If a request is submitted through POST, it is parsed and executed.
 * Otherwise only HEAD requests are supported.
 */
public class RequestHandler extends HttpServlet {
	
	@Override
	protected void doPost(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
		try {
			final Request request = RawRequest.parseRequest(req.getReader());
			Executor.execute(request);
			resp.setStatus(HttpServletResponse.SC_OK);
		} catch(final IOException | RequestParseException | RequestExecutionException e) {
			Logging.error("Failed to execute request", e);
			resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		}
	}


	@Override
	protected void doHead(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
		resp.setStatus(HttpServletResponse.SC_OK);
	}
}
