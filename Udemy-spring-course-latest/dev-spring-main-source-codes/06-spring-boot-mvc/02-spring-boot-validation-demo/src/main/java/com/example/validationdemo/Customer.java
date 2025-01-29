package com.example.validationdemo;

import com.example.validationdemo.validation.CourseCode;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

public class Customer {
	
	private String firstName;
	
	@NotNull(message="is required")
	@Size(min = 1, message="is required")
	private String lastName;
	
	@NotNull(message="is required")
	@Min(value = 0, message="must be greater than or equal to 0")
	@Max(value = 10, message="must be less than or equal to 10")
	//here we are using Integer instead of int because int is primitive and will not accept null values
	//Integer is wrapper class which is object and accepts null values so if we don't give any value in freePass then it will consider as null
	//we have given null should not be there by giving @NotNull(message="is required")
	private Integer freePass;
	
	@Pattern(regexp = "^[a-zA-Z0-9]{5}", message="only 5 char/digits allowed")
	private String postalCode;
	
	@CourseCode(value = "TOPS", message = "must start with TOPS")
	private String courseCode;

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public Integer getFreePass() {
		return freePass;
	}

	public void setFreePass(Integer freePass) {
		this.freePass = freePass;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getCourseCode() {
		return courseCode;
	}

	public void setCourseCode(String courseCode) {
		this.courseCode = courseCode;
	}
	
	
	
	
	
	
	
	

}
