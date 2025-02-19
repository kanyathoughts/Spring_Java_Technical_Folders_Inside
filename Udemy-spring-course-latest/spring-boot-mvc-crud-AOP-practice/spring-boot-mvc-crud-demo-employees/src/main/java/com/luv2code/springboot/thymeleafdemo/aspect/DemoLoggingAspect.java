package com.luv2code.springboot.thymeleafdemo.aspect;

import java.util.logging.Logger;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class DemoLoggingAspect {
	
	//setup logger
	private Logger myLogger = Logger.getLogger(getClass().getName());
	
	//setup poincut declarations
	@Pointcut("execution(* com.luv2code.springboot.thymeleafdemo.controller.*.*(..))")
	public void forControllerPackage() {}
	
	@Pointcut("execution(* com.luv2code.springboot.thymeleafdemo.service.*.*(..))")
	public void forServicePackage() {}
	
	@Pointcut("execution(* com.luv2code.springboot.thymeleafdemo.dao.*.*(..))")
	public void forDAOPackage() {}
	
	//combine pointcut expressions
	@Pointcut("forControllerPackage() || forServicePackage() || forDAOPackage()")
	public void forAppFlow() {}
	
	
	@Before("forAppFlow()")
	public void before(JoinPoint theJoinPoint) {
		//display method we are calling on
		String method = theJoinPoint.getSignature().toShortString();
		myLogger.info("===========> in @Before advice calling method is : " + method);
		
		//display arguments
		Object[] args = theJoinPoint.getArgs();
		for(Object arg:args) {
		myLogger.info("===========>> argument: " + arg);
		}
	}
	
	// add @AfterReturning advice
	@AfterReturning(
			pointcut="forAppFlow()",
			returning="result"
			)
	public void afterReturning(JoinPoint theJoinPoint, Object result) {
		//display method we are calling on
		String method = theJoinPoint.getSignature().toShortString();
		myLogger.info("===========> in @AfterReturning advice calling method is : " + method);
		
		//display the result
		myLogger.info("===========>> result: " + result);
		
	}
	

}
