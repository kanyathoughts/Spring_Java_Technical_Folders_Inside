package com.kafka.example.springbootkafka.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.kafka.example.springbootkafka.model.User;

@Service
public class JsonKafkaConsumer {
	private static final Logger LOGGER = LoggerFactory.getLogger(JsonKafkaConsumer.class);
	
	@KafkaListener(topics = "JsonTopic", groupId = "myGroup")
	public void consume(User data) {
		LOGGER.info(String.format("Json message receieved -> %s", data.toString()));
	}

}
