/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.event;

import static innowake.lib.core.lang.Assert.assertNotNull;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import com.hazelcast.cluster.Member;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.topic.ITopic;
import com.hazelcast.topic.Message;
import com.hazelcast.topic.MessageListener;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.internal.JobConfiguration;
import innowake.mining.data.event.ClusteredEvent;

/**
 * Service for broadcasting local application events on the Hazelcast cluster. Only events that extend {@link ClusteredEvent} will be broadcast.
 */
@Service
public class HazelcastEventBroadcastingService implements MessageListener<ClusteredEvent> {

	public static final String EVENT_BROADCASTING_TOPIC = "mining-event-broadcasting";

	@Autowired
	private ApplicationEventPublisher eventPublisher;

	@Autowired
	@Qualifier(JobConfiguration.HAZELCAST_INSTANCE)
	private HazelcastInstance hz;

	@Nullable
	private ITopic<ClusteredEvent> eventBroadcastingTopic;

	@Nullable
	private Member localMember;

	@PostConstruct
	public void init() {
		eventBroadcastingTopic = hz.getTopic(EVENT_BROADCASTING_TOPIC);
		localMember = hz.getCluster().getLocalMember();
		assertNotNull(eventBroadcastingTopic).addMessageListener(this);
	}

	@EventListener
	public void handleClusteredEvent(final ClusteredEvent event) {
		if (event.isToBePublishedToCluster()) {
			event.setPublishingMemberId(assertNotNull(localMember).getUuid());
			assertNotNull(eventBroadcastingTopic).publish(event);
		}
	}

	@Override
	public void onMessage(final @Nullable Message<ClusteredEvent> message) {
		/* we do not replay our own messages */
		if (message != null && ! message.getPublishingMember().equals(localMember)) {
			eventPublisher.publishEvent(message.getMessageObject());
		}
	}

}
