package com.love2code.springboot.demo.mycoolapp.rest;

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
		return "Hello World from Kanya";
	}
	
	//expose a new endpoint for workout
	
	@GetMapping("/workout")
	public int workOutMin() {
		return 10;
	}
	
	@GetMapping("/workoutnew")
	public int workOutMinutes() {
		return 12;
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
