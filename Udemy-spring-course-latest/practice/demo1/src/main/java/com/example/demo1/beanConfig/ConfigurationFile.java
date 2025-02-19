package com.example.demo1.beanConfig;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
public class ConfigurationFile {
	
	
	@Bean(name="VegPizzaBean")
	public Pizza vegPizza() {
		return new VegPizza();
	}
	
	@Bean
	public Pizza nonVegPizza() {
		return new NonVegPizza();
	}
	
	@Bean(initMethod = "init", destroyMethod = "destroy")
	public ControllerKanya controllerKanya() {
		return new ControllerKanya(nonVegPizza());
	}

}
