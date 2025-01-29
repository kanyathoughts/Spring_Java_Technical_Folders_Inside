package com.example.springcoredemo.common;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("prototype")
public class BaseBallCoach implements Coach{
	
	public BaseBallCoach() {
		System.out.println("In constructor: " + getClass().getSimpleName());
	}
	
	@Override
	public String getDailyWorkout() {
		return "This is Base ball coach :)";
	}

}
