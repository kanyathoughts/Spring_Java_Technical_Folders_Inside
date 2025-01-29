package com.example.unitTesting.UnitTesting.model;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Transient;

@Entity
public class Item {

	@Id
	int id;
	String name;
	int price;
	int quantity;
	// we don't want this field to be added into database so marked as transient field
	@Transient
	int value;
	
	
	public Item(int id, String name, int price, int quantity) {
		super();
		this.id = id;
		this.name = name;
		this.price = price;
		this.quantity = quantity;
	}
	
	
	public int getValue() {
		return value;
	}


	public void setValue(int value) {
		this.value = value;
	}


	public int getId() {
		return id;
	}
	public String getName() {
		return name;
	}
	public int getPrice() {
		return price;
	}
	public int getQuantity() {
		return quantity;
	}
	
	
	// For entity default constructor is mandatory
	public Item() {
		
	}
	
	
}
