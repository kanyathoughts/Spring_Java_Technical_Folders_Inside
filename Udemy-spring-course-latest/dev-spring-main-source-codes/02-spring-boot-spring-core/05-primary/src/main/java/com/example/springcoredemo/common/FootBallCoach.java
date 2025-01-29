package com.example.springcoredemo.common;

import org.springframework.stereotype.Component;

@Component
public class FootBallCoach implements Coach{
	
	@Override
	public String getDailyWorkout() {
		return "This is foot ball coach <3 :)";
	}

}
