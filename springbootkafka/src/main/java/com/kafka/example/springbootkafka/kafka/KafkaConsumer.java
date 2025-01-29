package com.kafka.example.springbootkafka.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
public class KafkaConsumer {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(KafkaConsumer.class);
	
	// This KafkaListener annotation subscribe the consumer to this topic and this consumer will be added to the "myGroup" group
	@KafkaListener(topics = "FirstTopic", groupId = "myGroup")
	public void consume(String message) {
		LOGGER.info(String.format("Message received -> %s", message));
	}

}
