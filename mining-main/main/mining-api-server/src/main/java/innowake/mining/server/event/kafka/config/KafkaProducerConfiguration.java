/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.event.kafka.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;
import org.springframework.kafka.support.serializer.JsonSerializer;

import java.util.Map;

/**
 * Kafka producer configuration providing {@link KafkaTemplate}.
 */
@Configuration
@ConditionalOnProperty({ "spring.kafka.bootstrap-servers", "kafka.enabled" })
public class KafkaProducerConfiguration {

	private final String bootstrapServers;

	public KafkaProducerConfiguration(@Value("${spring.kafka.bootstrap-servers}") final String bootstrapServers) {
		this.bootstrapServers = bootstrapServers;
	}

	@Bean
	public <T> KafkaTemplate<String, T> kafkaTemplate(final ObjectMapper objectMapper) {
		return new KafkaTemplate<>(kafkaProducerFactory(objectMapper));
	}

	@Bean
	public <T> ProducerFactory<String, T> kafkaProducerFactory(final ObjectMapper objectMapper) {
		final Map<String, Object> configProps = Map.of(
				ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers
		);
		return new DefaultKafkaProducerFactory<>(configProps, new StringSerializer(), new JsonSerializer<>(objectMapper));
	}
}
