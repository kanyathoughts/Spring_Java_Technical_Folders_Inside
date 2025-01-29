package com.kafka.example.springbootkafka.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.kafka.example.springbootkafka.kafka.JsonKafkaProducer;
import com.kafka.example.springbootkafka.model.User;

@RestController
@RequestMapping("/api/v1/kafka")
public class JsonMessageController {
	
	private JsonKafkaProducer jsonKafkaProducer;

	public JsonMessageController(JsonKafkaProducer jsonKafkaProducer) {
		super();
		this.jsonKafkaProducer = jsonKafkaProducer;
	}
	
	@PostMapping("/publish")
	public ResponseEntity<String> publish(@RequestBody User data) {
		jsonKafkaProducer.sendJsonMessage(data);
		return ResponseEntity.ok("Json Message sent to Kafka topic");
	}

}
