package com.luv2code.example.AOPExample.dao;

import com.luv2code.example.AOPExample.Account;

public interface AccountDAO {
	
	void addAccount();
	
	void addAccount(Account theAccount, boolean vipFlag);
	
	boolean doWork();

}
