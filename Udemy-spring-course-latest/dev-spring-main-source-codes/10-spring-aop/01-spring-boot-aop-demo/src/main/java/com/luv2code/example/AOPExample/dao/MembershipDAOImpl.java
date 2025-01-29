package com.luv2code.example.AOPExample.dao;

import org.springframework.stereotype.Repository;

@Repository
public class MembershipDAOImpl implements MembershipDAO{
	

	@Override
	public boolean addSillyMember() {
		System.out.println(getClass() + ": DOING MY DB WORK..ADDING A MEMBERSHIP for silly member");
		return true;
	}

	@Override
	public void goToSleep() {
		System.out.println("Just go to sleep!!");
		
	}

}
