package com.example.demo.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class FunRestController {
	
	//Inject properties of coach.name and team.name from application.properties file
	@Value("${coach.name}")
	public String coachName;
		
	@Value("${team.name}")
	public String teamName;
	
	@GetMapping("/")
	public String sayHello() {
		return "Hello world!";
	}
	
	
	@GetMapping("/coach")
	public String getCoach() {
		return coachName;
	}
	
	@GetMapping("/team")
	public String getTeam() {
		return teamName;
	}
	

}
