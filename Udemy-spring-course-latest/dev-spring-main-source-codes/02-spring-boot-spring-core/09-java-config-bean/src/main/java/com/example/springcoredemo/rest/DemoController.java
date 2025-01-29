package com.example.springcoredemo.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.springcoredemo.common.Coach;

@RestController
public class DemoController {
	
	//define a private field for the dependency
	private Coach mycoach;
	
	
	@Autowired
	public DemoController(@Qualifier("swimCoachCustom") Coach theCoach) {
		System.out.println("In constructor: " + getClass().getSimpleName());
		mycoach = theCoach;
	}
	
	
	
	@GetMapping("/dailyworkout")
	public String getDailyWorkOut() {
		return mycoach.getDailyWorkout();
	}
}
