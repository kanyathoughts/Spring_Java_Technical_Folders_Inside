package com.example.demo1.beanConfig;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

public class ControllerKanya {
	
	private Pizza pizza;

//	@Autowired
	public ControllerKanya(Pizza pizza) {
		this.pizza = pizza;
	}
	
	public void getPizzaFromController() {
		pizza.getPizza();
	}
	
	public void init() {
		System.out.println("Initialization logic");
	}
	
	public void destroy() {
		System.out.println("destruction logic");
	}
	
	
	
	
	

}
