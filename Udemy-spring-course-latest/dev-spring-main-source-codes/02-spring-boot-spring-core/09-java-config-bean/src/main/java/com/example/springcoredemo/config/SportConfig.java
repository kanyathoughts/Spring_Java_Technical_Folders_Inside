package com.example.springcoredemo.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.example.springcoredemo.common.Coach;
import com.example.springcoredemo.common.SwimCoach;

@Configuration
public class SportConfig {
	
	
	//swimCoach is a bean id
	//we have not given @Component  in SwimCoach class so here we are manually configuring SwimCoach as a bean.
	@Bean("swimCoach")
	public Coach swimCoach() {
		return new SwimCoach();
	}
	
	@Bean("swimCoachCustom")
	public Coach swimCoachCustom() {
		return new SwimCoach("kanya");
	}
	

}
