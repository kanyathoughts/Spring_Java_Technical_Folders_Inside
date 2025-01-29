/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.externalparsing;

import com.hazelcast.collection.IQueue;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.hazelcast.multimap.MultiMap;
import innowake.mining.server.discovery.externalparsing.rpc.HazelcastRpcTransport;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

@TestPropertySource(properties = "external-parsing.enabled=true")
@SpringBootTest(classes = { ExternalParsingConfiguration.class, JacksonAutoConfiguration.class })
class ExternalParsingServiceTest {

	@Autowired
	private ExternalParsingService externalParsingService;

	@Autowired
	@Qualifier("external-parsing")
	private HazelcastInstance hz;

	private final ExecutorService workerExecutor = Executors.newCachedThreadPool();

	@Test
	void testParseFileSuccess() throws InterruptedException, TimeoutException, ExecutionException {
		final String testProviderId = "testProviderId";
		final Future<?> workerFuture = workerExecutor.submit(createTestWorker(testProviderId, false));

		final Map<String, Object> parameters = new HashMap<>();
		final ExternalParsingResult<String> result = externalParsingService.parseFile(testProviderId, parameters, 10, TimeUnit.SECONDS, String.class);

		/* assert worker has finished cleanly */
		workerFuture.get();

		assertEquals(ExternalParsingResult.Status.OK, result.getStatus());
		assertEquals("Hello, World!", result.getResult());
	}

	@Test
	void testParseFileError() throws InterruptedException, TimeoutException, ExecutionException {
		final String testProviderId = "testProviderId";
		final Future<?> workerFuture = workerExecutor.submit(createTestWorker(testProviderId, true));

		final Map<String, Object> parameters = new HashMap<>();
		final ExternalParsingResult<String> result = externalParsingService.parseFile(testProviderId, parameters, 10, TimeUnit.SECONDS, String.class);

		/* assert worker has finished cleanly */
		workerFuture.get();

		assertEquals(ExternalParsingResult.Status.ERROR, result.getStatus());
		assertEquals("Test Error Message", result.getErrorMessage());
	}

	@Test
	void testParseFileNoProvider() throws InterruptedException, TimeoutException {
		final Map<String, Object> parameters = new HashMap<>();
		final ExternalParsingResult<String> result = externalParsingService.parseFile("noProvider", parameters, 10, TimeUnit.SECONDS, String.class);

		assertEquals(ExternalParsingResult.Status.NOT_AVAILABLE, result.getStatus());
	}

	@Test
	void testParseFileTimeout() {
		/* here we "simulate" having a provider by manually adding it to the provider map, but don't actually send a response, leading to timeout */
		final String testProviderId = "testProviderId";
		final MultiMap<String, String> providerMap = hz.getMultiMap(HazelcastRpcTransport.MEMBER_MAP_NAME);
		providerMap.put(hz.getCluster().getLocalMember().getUuid().toString(), testProviderId);

		final Map<String, Object> parameters = new HashMap<>();
		assertThrows(TimeoutException.class, () -> externalParsingService.parseFile(testProviderId, parameters, 1, TimeUnit.SECONDS, String.class));
	}

	private Callable<Void> createTestWorker(final String providerId, final boolean produceError) {
		return () -> {
			final MultiMap<String, String> providerMap = hz.getMultiMap(HazelcastRpcTransport.MEMBER_MAP_NAME);
			providerMap.put(hz.getCluster().getLocalMember().getUuid().toString(), providerId);

			final IQueue<Map<String, Object>> requestQueue = hz.getQueue(HazelcastRpcTransport.REQUEST_QUEUE_PREFIX + providerId);
			final IMap<String, Map<String, Object>> responsesMap = hz.getMap(HazelcastRpcTransport.RESPONSES_MAP_NAME);

			final Map<String, Object> request = requestQueue.take();
			assertEquals("2.0", request.get("jsonrpc"), "request should have valid 'jsonrpc' field");
			final Object id = request.get("id");
			assertNotNull(id, "request should have an id");

			final Map<String, Object> response = new HashMap<>();
			response.put("jsonrpc", "2.0");
			response.put("id", id);
			if (produceError) {
				final Map<String, Object> error = new HashMap<>();
				error.put("code", ExternalParsingResult.ErrorCode.INTERNAL_ERROR.getErrorNumber());
				error.put("message", "Test Error Message");
				response.put("error", error);
			} else {
				response.put("result", "Hello, World!");
			}

			responsesMap.put(id.toString(), response);

			return null;
		};
	}
}
