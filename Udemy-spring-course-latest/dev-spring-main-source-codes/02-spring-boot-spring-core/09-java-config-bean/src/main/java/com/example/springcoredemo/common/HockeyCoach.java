package com.example.springcoredemo.common;

import org.springframework.stereotype.Component;

@Component
public class HockeyCoach implements Coach{
	
	public HockeyCoach() {
		System.out.println("In constructor: " + getClass().getSimpleName());
	}

	@Override
	public String getDailyWorkout() {
		return "This is hockey coach <3 :)";
	}

}
