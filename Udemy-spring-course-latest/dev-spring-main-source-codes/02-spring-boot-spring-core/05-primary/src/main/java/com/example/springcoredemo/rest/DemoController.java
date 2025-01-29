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
	
	//We can specify which qualifier we want by specifying same class name, first character lower-case
	//eg: HockeyCoach but bean name is hockeyCoach
	//One alternative of @Qualifier is @primary  and if we specify @primary to multiple classes then we will get error so we should not give primary to more than one calss
//@Qualifier has more priority than @Primary and we can mix up using both Qualifier and @Primary bust Qualifier has high priority.
	@Autowired
	public DemoController(@Qualifier("baseBallCoach") Coach theCoach) {
		mycoach = theCoach;
	}
	
	@GetMapping("/dailyworkout")
	public String getDailyWorkOut() {
		return mycoach.getDailyWorkout();
	}
}
