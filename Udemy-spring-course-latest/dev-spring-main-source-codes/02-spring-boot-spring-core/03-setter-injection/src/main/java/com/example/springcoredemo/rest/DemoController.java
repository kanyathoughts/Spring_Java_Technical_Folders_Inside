package com.example.springcoredemo.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.springcoredemo.common.Coach;

@RestController
public class DemoController {
	
	//define a private field for the dependency
	//We can directly mention @Autowired on field and we don't need constructor/setter injections but it's not recommended 
	//because it will make the code hard in unit testing.
	//@Autowired
	private Coach mycoach;
	
	//Setter injection using setter method and we can use any other names since we are mentioning @Autowired
	@Autowired
	public void setCoach(Coach theCoach) {
		mycoach = theCoach;
	}
	
	@GetMapping("/dailyworkout")
	public String getDailyWorkOut() {
		return mycoach.getDailyWorkout();
	}
}
