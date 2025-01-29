package com.example.cruddemo.dao;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.example.cruddemo.entity.Employee;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;

@Repository
public class EmployeeDAOImpl implements EmployeeDAO {
	
	private EntityManager entityManager;
	
	@Autowired
	public EmployeeDAOImpl(EntityManager entityManager) {
		this.entityManager = entityManager;
	}

	@Override
	public List<Employee> findAll() {
		TypedQuery<Employee> theQuery = entityManager.createQuery("FROM Employee", Employee.class);
		return theQuery.getResultList();
	}

	@Override
	public Employee findById(int id) {
		return entityManager.find(Employee.class, id);
	}

	@Override
	public Employee save(Employee theEmployee) {
		//save employee
		//if id=0 then save/insert else update
		Employee emp = entityManager.merge(theEmployee);
		return emp;
	}

	@Override
	public void deleteById(int id) {
		Employee emp = entityManager.find(Employee.class, id);
		if (emp == null) {
			throw new RuntimeException("employee id is not exists: " + id);
		}
		entityManager.remove(emp);
	}

	@Override
	public void add(Employee employee) {
		entityManager.persist(employee);
	}

}
