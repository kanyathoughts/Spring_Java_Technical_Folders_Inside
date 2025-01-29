package com.example.springcoredemo.common;

import org.springframework.stereotype.Component;

@Component
public class HockeyCoach implements Coach{

	@Override
	public String getDailyWorkout() {
		return "This is hockey coach <3 :)";
	}

}
