package com.kafka.example.springbootkafka.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

import com.kafka.example.springbootkafka.model.User;

@Service
public class JsonKafkaProducer {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(JsonKafkaProducer.class);
	
	// Spring boot will provide auto configuration for kafka template so that means already 
	// we have bean in spring context so we just need to inject the dependency
	private KafkaTemplate<String, User> kafkaTemplate;
		
	public JsonKafkaProducer(KafkaTemplate<String, User> kafkaTemplate) {
		this.kafkaTemplate = kafkaTemplate;
	}
	
	public void sendJsonMessage(User data) {
		Message<User> message = MessageBuilder.withPayload(data).setHeader(KafkaHeaders.TOPIC, "JsonTopic").build();
		kafkaTemplate.send(message);
		LOGGER.info(String.format("Message sent -> %s", data.toString()));
	}

}
