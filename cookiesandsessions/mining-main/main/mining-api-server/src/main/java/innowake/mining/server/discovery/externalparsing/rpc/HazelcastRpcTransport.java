/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.externalparsing.rpc;

import com.hazelcast.cluster.MembershipAdapter;
import com.hazelcast.cluster.MembershipEvent;
import com.hazelcast.collection.IQueue;
import com.hazelcast.core.EntryAdapter;
import com.hazelcast.core.EntryEvent;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.hazelcast.multimap.MultiMap;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * JSON-RPC transport implementation using Hazelcast.
 * <p>
 * This implementation uses one {@code IQueue} per provider id, so multiple parse providers
 * with the same id (i.e. for the same type of files) can poll the queue for load balancing.
 * All responses from all providers (regardless of id) are collected in a single response map,
 * using the JSON-RPC request id as key. Responses are removed from the Map when consumed.
 * Otherwise they are automatically removed after a configured TTL.
 * </p>
 */
@Service
public class HazelcastRpcTransport {

	private static final Logger LOG = LoggerFactory.getLogger(HazelcastRpcTransport.class);

	public static final String MEMBER_MAP_NAME = "external-parsing-members";
	public static final String RESPONSES_MAP_NAME = "external-parsing-responses";
	public static final String REQUEST_QUEUE_PREFIX = "external-parsing-request-";
	public static final int RESPONSE_TTL = 300; /* seconds */

	private static final String TIMEOUT_ERROR_MESSAGE = "External parsing request %s timed out.";

	private final ConcurrentMap<String, Pair<Lock, Condition>> activeRequests = new ConcurrentHashMap<>();
	private final HazelcastInstance hz;
	private final MultiMap<String, String> memberMap;
	private final IMap<String, Map<String, Object>> responsesMap;

	@Autowired
	public HazelcastRpcTransport(@Qualifier("external-parsing") final HazelcastInstance hz) {
		this.hz = hz;
		memberMap = hz.getMultiMap(MEMBER_MAP_NAME);
		responsesMap = hz.getMap(RESPONSES_MAP_NAME);
		hz.getCluster().addMembershipListener(new MembershipAdapter() {
			@Override
			public void memberRemoved(final MembershipEvent membershipEvent) {
				memberMap.remove(membershipEvent.getMember().getUuid());
			}
		});
		responsesMap.addEntryListener(new EntryAdapter<String, Map<String, Object>>() {
			@Override
			public void entryAdded(final EntryEvent<String, Map<String, Object>> event) {
				LOG.debug(() -> "Received response to external parsing request " + event.getKey());
				final Pair<Lock, Condition> requestLock = activeRequests.get(event.getKey());
				if (requestLock == null) {
					LOG.debug(() -> "No local request matches response id " + event.getKey());
					/* no active request that matches this response */
					return;
				}
				requestLock.getLeft().lock();
				try {
					LOG.debug(() -> "Delivering response for external parsing request " + event.getKey());
					requestLock.getRight().signalAll();
				} finally {
					requestLock.getLeft().unlock();
				}
			}
		}, false);
	}

	/**
	 * Checks if a provider that can handle external parsing requests with the given id is available.
	 *
	 * @param providerId the id of the external parsing provider
	 * @return {@code true} if at least one provider is available, else {@code false}
	 */
	public boolean hasProvider(final String providerId) {
		return memberMap.values().contains(providerId);
	}

	/**
	 * Makes a request to an external parsing provider via RPC. This method blocks until a response is available
	 * or the given timeout has elapsed. In the latter case a {@link TimeoutException} is thrown.
	 *
	 * @param providerId id of the external parsing provider to call
	 * @param requestObject object containing the JSON-RPC request
	 * @param timeout maximum time to wait for a response before {@link TimeoutException} is thrown
	 * @param timeUnit time unit for {@code timeout} parameter
	 *
	 * @return the JSON-RPC response
	 * @throws InterruptedException when current Thread is interrupted while waiting for the response
	 * @throws TimeoutException when the timeout elapses without receiving a response
	 */
	public Map<String, Object> request(final String providerId, final Map<String, Object> requestObject,
			final long timeout, final TimeUnit timeUnit) throws InterruptedException, TimeoutException {
		final String id = (String) requestObject.get("id");

		if (id == null) {
			/* requests without 'id' are legal according to JSON-RPC spec for "notifications", but we don't support this right now */
			throw new IllegalArgumentException("JSON-RPC request is missing an 'id' property.");
		}

		final Pair<Lock, Condition> requestLock = activeRequests.computeIfAbsent(id, (key) -> {
			final Lock lock = new ReentrantLock();
			return Pair.of(lock, lock.newCondition());
		});

		final Instant start = Instant.now();
		requestLock.getLeft().lock();
		try {
			final IQueue<Map<String, Object>> requestQueue = hz.getQueue(REQUEST_QUEUE_PREFIX + providerId);
			requestQueue.put(requestObject);
			while ( ! responsesMap.containsKey(id)) {
				final long elapsed = start.until(Instant.now(), timeUnit.toChronoUnit());
				if (elapsed >= timeout) {
					LOG.warn(() -> String.format(TIMEOUT_ERROR_MESSAGE, id));
					throw new TimeoutException(String.format(TIMEOUT_ERROR_MESSAGE, id));
				}
				if ( ! requestLock.getRight().await(timeout - elapsed, timeUnit)) {
					LOG.warn(() -> String.format(TIMEOUT_ERROR_MESSAGE, id));
					throw new TimeoutException(String.format(TIMEOUT_ERROR_MESSAGE, id));
				}
			}
			return responsesMap.remove(id);
		} finally {
			requestLock.getLeft().unlock();
			activeRequests.remove(id);
		}
	}
}
