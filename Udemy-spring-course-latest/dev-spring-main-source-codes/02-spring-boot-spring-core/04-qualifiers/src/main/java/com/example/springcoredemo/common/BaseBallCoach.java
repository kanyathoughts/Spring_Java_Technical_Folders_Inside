package com.example.springcoredemo.common;

import org.springframework.stereotype.Component;

@Component
public class BaseBallCoach implements Coach{

	@Override
	public String getDailyWorkout() {
		return "This is Base ball coach :)";
	}

}
