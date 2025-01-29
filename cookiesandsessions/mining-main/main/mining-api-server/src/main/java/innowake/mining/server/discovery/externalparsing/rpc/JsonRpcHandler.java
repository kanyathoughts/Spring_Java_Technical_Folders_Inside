/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.externalparsing.rpc;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.externalparsing.ExternalParsingResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Handler of JSON-RPC requests to external parsing providers.
 */
@Service
public class JsonRpcHandler {

	private static final Logger LOG = LoggerFactory.getLogger(JsonRpcHandler.class);

	private final HazelcastRpcTransport rpcTransport;
	private final ObjectMapper objectMapper;

	@Autowired
	public JsonRpcHandler(final HazelcastRpcTransport rpcTransport, final ObjectMapper objectMapper) {
		this.rpcTransport = rpcTransport;
		this.objectMapper = objectMapper;
	}

	/**
	 * Sends a JSON-RPC request to an external parsing provider. This method blocks until a response is available
	 * or the given timeout has elapsed. In the latter case a {@link TimeoutException} is thrown.
	 * If no provider is available for the given {@code providerId}, then this method returns
	 * {@linkplain ExternalParsingResult#forNotAvailable() not available}.
	 *
	 * @param providerId id of the external parsing provider to call
	 * @param method name of the RPC method to invoke
	 * @param parameters parameters for the RPC method call
	 * @param timeout maximum time to wait for a response before {@link TimeoutException} is thrown
	 * @param timeUnit time unit for {@code timeout} parameter
	 * @param resultClass expected result type
	 *
	 * @return the parsed response as {@link ExternalParsingResult}
	 * @throws InterruptedException when current Thread is interrupted while waiting for the response
	 * @throws TimeoutException when the timeout elapses without receiving a response
	 */
	public <T> ExternalParsingResult<T> request(final String providerId, final String method, final Map<String, Object> parameters,
			final long timeout, final TimeUnit timeUnit, final Class<T> resultClass) throws InterruptedException, TimeoutException {

		if ( ! rpcTransport.hasProvider(providerId)) {
			return ExternalParsingResult.forNotAvailable();
		}

		final String id = UUID.randomUUID().toString();
		final Map<String, Object> requestObject = new HashMap<>();
		requestObject.put("jsonrpc", "2.0");
		requestObject.put("id", id);
		requestObject.put("method", method);
		requestObject.put("params", parameters);

		final Map<String, Object> response = rpcTransport.request(providerId, requestObject, timeout, timeUnit);
		LOG.trace(() -> String.format("Received RPC response %s", response));

		if (response.containsKey("result")) {
			return ExternalParsingResult.forSuccess(objectMapper.convertValue(response.get("result"), resultClass));
		} else {
			@SuppressWarnings("unchecked")
			final Map<String, Object> error = (Map<String, Object>) response.get("error");
			final ExternalParsingResult.ErrorCode errorCode = ExternalParsingResult.ErrorCode.fromErrorCode(Integer.parseInt(error.get("code").toString()));
			final String errorMessage = (String) error.get("message");
			@SuppressWarnings("unchecked")
			final Map<String, Object> data = (Map<String, Object>) error.get("data");

			return ExternalParsingResult.forError(errorCode, errorMessage, data);
		}
	}
}
