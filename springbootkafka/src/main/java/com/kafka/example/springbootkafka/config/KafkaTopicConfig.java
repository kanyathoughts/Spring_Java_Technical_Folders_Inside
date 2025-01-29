package com.kafka.example.springbootkafka.config;

import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.TopicBuilder;

@Configuration
public class KafkaTopicConfig {
	
	@Bean
	public NewTopic getFirstTopic() {
		return TopicBuilder.name("FirstTopic").build();
	}
	
	@Bean
	public NewTopic getJsonTopic() {
		return TopicBuilder.name("JsonTopic").build();
	}

}
