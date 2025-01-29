package com.rest_demo.example.rest_demo;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class StudentController {
	
	@Autowired
	private StudentRepo repo;
	
	@GetMapping("/students")
	public List<Student> getStudents() {
		return repo.findAll();
	}

}
