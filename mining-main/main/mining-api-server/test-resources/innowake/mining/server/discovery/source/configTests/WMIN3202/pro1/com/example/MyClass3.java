package com.example;

import in.example.MyClass4;

public class MyClass3 {
	public void example() {
		new MyClass2(); /* calling a class from same project */
		new MyClass4(); /* calling a class from a different project but its a duplicate */
	}
}