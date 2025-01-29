package com.example.springcoredemo;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class DemoController {
	
	//define a private field for the dependency
	private Coach mycoach;
	
	//define a constructor for dependency injection
	//Autowired tells spring to inject dependencies and it will look for all @components which implements the Coach interface
	//If only one constructor is present Autowired is not required and it's optional.
	@Autowired
	public DemoController(Coach theCoach) {
		mycoach = theCoach;
	}
	
	@GetMapping("/dailyworkout")
	public String getDailyWorkOut() {
		return mycoach.getDailyWorkout();
	}
}
