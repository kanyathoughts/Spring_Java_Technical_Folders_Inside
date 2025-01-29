package com.luv2code.example.AOPExample.aspect;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class MyDemoLoggingAspect {
	
	// This is where we add all of our logging related advices
	
	// Let's start with an @Before advice
	
	// Run this code before target object method which is public void addAccount() method
	// Inside the quotes is called pointcut expression, tells that where this before advice should be applied
	
	// This will match addAccount method in any class
	// @Before("execution(public void addAccount())")
	
	// This will match addAccount method in AccountDAO interface only
	// @Before("execution(public void com.luv2code.example.AOPExample.dao.AccountDAO.addAccount())")
	
	// This will match any method, name which starts with add
	// Here we are using wild card pattern to match method name
	// @Before("execution(public void add*())")
	
	// This will match any method, name which starts with add
	// Basically modifiers is optional so we removed it
	// specified return type as void
	// @Before("execution(void add*())")
	
	// Let's add wild card character to simply check any return type would be fine and here * means any return type
	// @Before("execution(* add*())")
	
	
	// Here we are giving fully qualified name of the class name which is parameter type
	// We must give fully qualified name for the class name as parameter type
	// @Before("execution(* add*(Account))") //This will not work and throw illegalArgumentException
	// @Before("execution(* add*(com.luv2code.example.AOPExample.Account))")
	
	// This will match account object followed by any number of arguments
	// @Before("execution(* add*(com.luv2code.example.AOPExample.Account, ..))")
	
	// This will match zero or many number of parameters
	//@Before("execution(* add*(..))")
	
	// This will match any return type
	// we have specified package name com.luv2code.example.AOPExample.dao
	// then .* which means any class inside this package
	// then .* then any method inside this package
	// then inside parenthesis we have .. which means 0 to many parameters
	@Before("execution(* com.luv2code.example.AOPExample.dao.*.*(..))")
	//This below method name can be anything
	public void beforeAddAccountAdvice() {
		
		System.out.println("\n ==========>>> executing @Before advice on method");
		
	}
}
