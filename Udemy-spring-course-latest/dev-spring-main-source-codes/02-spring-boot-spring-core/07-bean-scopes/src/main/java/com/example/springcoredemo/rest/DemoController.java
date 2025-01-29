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
	private Coach anothercoach;
	
	
	@Autowired
	public DemoController(@Qualifier("baseBallCoach") Coach theCoach, @Qualifier("baseBallCoach") Coach theAnothetCoach) {
		System.out.println("In constructor: " + getClass().getSimpleName());
		mycoach = theCoach;
		anothercoach = theAnothetCoach;
	}
	
	@GetMapping("/dailyworkout")
	public String getDailyWorkOut() {
		return "checking bean scope: " + (mycoach == anothercoach);
	}
}
