/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.event.kafka.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.support.serializer.JsonDeserializer;

import java.util.Map;

/**
 * Kafka consumer configuration providing the default {@link org.springframework.kafka.config.KafkaListenerContainerFactory}.
 */
@Configuration
@ConditionalOnProperty({ "spring.kafka.bootstrap-servers", "kafka.enabled" })
public class KafkaConsumerConfiguration {

	private final String bootstrapServers;
	private final String groupId;

	public KafkaConsumerConfiguration(@Value("${spring.kafka.bootstrap-servers}") final String bootstrapServers,
									  @Value("${kafka.group-id}") final String groupId) {
		this.bootstrapServers = bootstrapServers;
		this.groupId = groupId;
	}

	@Bean
	public <T> ConcurrentKafkaListenerContainerFactory<String, T> kafkaListenerContainerFactory(final ObjectMapper objectMapper) {
		final ConcurrentKafkaListenerContainerFactory<String, T> containerFactory = new ConcurrentKafkaListenerContainerFactory<>();
		containerFactory.setConsumerFactory(kafkaConsumerFactory(objectMapper));
		return containerFactory;
	}

	@Bean
	public <T> ConsumerFactory<String, T> kafkaConsumerFactory(final ObjectMapper objectMapper) {
		final Map<String, Object> props = Map.of(
				ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers,
				ConsumerConfig.GROUP_ID_CONFIG, groupId
		);
		return new DefaultKafkaConsumerFactory<>(props, new StringDeserializer(), new JsonDeserializer<>(objectMapper));
	}

}
