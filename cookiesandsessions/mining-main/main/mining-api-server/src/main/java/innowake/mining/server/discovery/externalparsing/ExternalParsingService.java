/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.externalparsing;


import innowake.mining.server.discovery.externalparsing.rpc.JsonRpcHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Service for making requests to external parsing providers.
 */
@Service
public class ExternalParsingService {

	public static final String PARSE_FILE_METHOD_NAME = "externalParsing/parseFile";

	private final JsonRpcHandler rpcHandler;

	@Autowired
	public ExternalParsingService(final JsonRpcHandler rpcHandler) {
		this.rpcHandler = rpcHandler;
	}

	/**
	 * Requests an external parsing provider with the given {@code providerId} to parse a file.
	 * This method blocks until a response is available
	 * or the given timeout has elapsed. In the latter case a {@link TimeoutException} is thrown.
	 * If no provider is available for the given {@code providerId}, then this method returns
	 * {@linkplain ExternalParsingResult#forNotAvailable() not available}.
	 *
	 * @param providerId id of the external parsing provider
	 * @param parameters provider-specific parameters for the parse request
	 * @param timeout maximum time to wait for a response before {@link TimeoutException} is thrown
	 * @param timeUnit time unit for {@code timeout} parameter
	 * @param resultClass expected result type
	 *
	 * @return the parse result as {@link ExternalParsingResult}
	 * @throws InterruptedException when current Thread is interrupted while waiting for the response
	 * @throws TimeoutException when the timeout elapses without receiving a response
	 */
	public <T> ExternalParsingResult<T> parseFile(final String providerId, final Map<String, Object> parameters,
			final long timeout, final TimeUnit timeUnit, final Class<T> resultClass) throws InterruptedException, TimeoutException {

		return rpcHandler.request(providerId, PARSE_FILE_METHOD_NAME, parameters, timeout, timeUnit, resultClass);
	}
}
