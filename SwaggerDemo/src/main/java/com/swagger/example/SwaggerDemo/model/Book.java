package com.swagger.example.SwaggerDemo.model;

import java.io.Serializable;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Table;


@Entity
@Table
public class Book implements Serializable{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1760935688969211133L;
	@Id
	@GeneratedValue
	private int bookId;
	private String bookName;
	private double price;
	public int getBookId() {
		return bookId;
	}
	public void setBookId(int bookId) {
		this.bookId = bookId;
	}
	public String getBookName() {
		return bookName;
	}
	public void setBookName(String bookName) {
		this.bookName = bookName;
	}
	public double getPrice() {
		return price;
	}
	public void setPrice(double price) {
		this.price = price;
	}
	public Book(String bookName, double price) {
		super();
		this.bookName = bookName;
		this.price = price;
	}
	
	
	public Book() {
		
	}

}
