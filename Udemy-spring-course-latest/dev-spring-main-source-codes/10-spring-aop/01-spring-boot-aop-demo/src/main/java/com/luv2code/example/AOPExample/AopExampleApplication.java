package com.luv2code.example.AOPExample;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import com.luv2code.example.AOPExample.dao.AccountDAO;
import com.luv2code.example.AOPExample.dao.MembershipDAO;

@SpringBootApplication
public class AopExampleApplication {

	public static void main(String[] args) {
		SpringApplication.run(AopExampleApplication.class, args);
	}
	
	@Bean
	public CommandLineRunner commandLineRunner(AccountDAO theAccountDAO, MembershipDAO theMembershipDAO) {
		return runner -> {
			demoTheBeforeAdvice(theAccountDAO, theMembershipDAO);
		};
	}

	private void demoTheBeforeAdvice(AccountDAO theAccountDAO, MembershipDAO theMembershipDAO) {
		theAccountDAO.addAccount();
		
		
//		//do it again
//		System.out.print("\n let's call it again! \n");
//		
//		theAccountDAO.addAccount();
		
		// when we actually call a method then only point cut expression would try to execute and apply the advice here
		
		//call the membership DAO addAccount
		
		theMembershipDAO.addSillyMember();
		
		theAccountDAO.addAccount(new Account("kanya", "DC Engineer 1"), true);
		
		theAccountDAO.doWork();
		
		theMembershipDAO.goToSleep();
	}

}
