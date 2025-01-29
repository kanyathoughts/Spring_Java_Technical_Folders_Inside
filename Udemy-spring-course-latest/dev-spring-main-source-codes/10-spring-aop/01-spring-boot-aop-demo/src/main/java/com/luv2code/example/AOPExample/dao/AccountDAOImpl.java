package com.luv2code.example.AOPExample.dao;

import org.springframework.stereotype.Repository;

import com.luv2code.example.AOPExample.Account;

@Repository
public class AccountDAOImpl implements AccountDAO{
	
	

	@Override
	public void addAccount() {
		System.out.println(getClass() + ": DOING MY DB WORK..ADDING AN ACCOUNT");
		
	}

	@Override
	public void addAccount(Account theAccount, boolean vipFlag) {
		System.out.println("The added account is: ");
		System.out.println(theAccount.toString());
		
	}

	@Override
	public boolean doWork() {
		System.out.println("doWork()!!");
		return true;
	}

}
