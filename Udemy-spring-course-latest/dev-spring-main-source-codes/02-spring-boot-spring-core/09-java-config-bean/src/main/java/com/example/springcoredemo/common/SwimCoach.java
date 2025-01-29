package com.example.springcoredemo.common;


public class SwimCoach implements Coach{
	
	public SwimCoach() {
		System.out.println("In constructor: " + getClass().getSimpleName());
	}
	
	public SwimCoach(String player) {
		System.out.println("In constructor: " + getClass().getSimpleName() + " -> player: " + player);
	}
	
	@Override
	public String getDailyWorkout() {
		return "Swim coach <3 :)";
	}

}
