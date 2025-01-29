package com.demo;

public class Class10 {
	
	String myMethod5(int i, String... s) {
		return i + "" + s;
	}
	
	String ss = myMethod5(0, "Hello");
}
