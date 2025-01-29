package com.example.cruddemo.dao;

import java.util.List;

import com.example.cruddemo.entity.Employee;

public interface EmployeeDAO {
	
	List<Employee> findAll();
	
	Employee findById(int id);
	
	//This is for both inserting and updating
	Employee save(Employee theEmployee);
	
	void deleteById(int id);
	
	void add(Employee employee);

}
