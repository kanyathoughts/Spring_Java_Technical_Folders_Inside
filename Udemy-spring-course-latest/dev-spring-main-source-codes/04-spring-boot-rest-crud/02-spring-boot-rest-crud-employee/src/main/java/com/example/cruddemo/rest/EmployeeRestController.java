package com.example.cruddemo.rest;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.cruddemo.dao.EmployeeDAO;
import com.example.cruddemo.entity.Employee;
import com.example.cruddemo.service.EmployeeService;

@RestController
@RequestMapping("/api")
public class EmployeeRestController {
	
	//injecting employee service instead of direct DAO
	private EmployeeService employeeService;
	
	@Autowired
	public EmployeeRestController(EmployeeService employeeService) {
		this.employeeService = employeeService;
	}



//	//quick and dirty : inject employee dao
//	private EmployeeDAO employeeDAO;
//
//	@Autowired
//	public EmployeeRestController(EmployeeDAO employeeDAO) {
//		this.employeeDAO = employeeDAO;
//	}
	
	//expose "/employees" and return list of employees
	@GetMapping("/employees")
	public List<Employee> getEmployees() {
		return employeeService.findAll();
	}
	
	@GetMapping("/employees/{employeeId}")
	public Employee getEmployeeById(@PathVariable int employeeId) {
		Employee theEmployee = employeeService.findById(employeeId);
		if(theEmployee == null) {
			throw new RuntimeException("Employee id not found: " + employeeId);
		}
		return theEmployee;
	}
	
	@PostMapping("/employees")
	public List<Employee> addEmployeeNew(@RequestBody Employee employee) {
		employeeService.add(employee);
		return employeeService.findAll();
	}
	
	//It is also working we can use save method or add method to insert new employee
//	@PostMapping("/employees")
//	public Employee addEmployee(@RequestBody Employee theEmployee) {
//		
//		//also just in case they pass an id in json .... set id to 0
//		//this is to force a save of new item .... instead of update
//		
//		theEmployee.setId(0);
//		
//		Employee employee = employeeService.save(theEmployee);
//		return employee;
//	}
	
	//It is also working for updating employee but such a lonf process here so don't prefer this but keeping this only for knowledge
//	@PutMapping("/employees/{employeeId}")
//	public Employee updateEmployee(@RequestBody Employee theEmployee, @PathVariable int employeeId) {
//		Employee employee = employeeService.findById(employeeId);
//		if(employee == null) {
//			throw new RuntimeException("employee id is not available: " + employeeId);
//		}
//		employee.setFirstName(theEmployee.getFirstName());
//		employee.setLastName(theEmployee.getLastName());
//		employee.setEmail(theEmployee.getEmail());
//		Employee newEmployee = employeeService.save(employee);
//		return newEmployee;
//	}
	
	@PutMapping("/employees")
	public Employee updateEmployee(@RequestBody Employee theEmployee) {
		Employee employee = employeeService.save(theEmployee);
		return employee;
	}
	
	@DeleteMapping("/employees/{employeeId}")
	public String deleteEmployee(@PathVariable int employeeId) {
		employeeService.deleteById(employeeId);
		return "Deleted employee by id: " + employeeId;
	}
	
}
