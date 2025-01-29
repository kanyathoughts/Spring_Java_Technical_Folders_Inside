/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository;

import java.util.Collection;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import innowake.mining.shared.springdata.annotations.Query;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Designation;

/**
 * Repository to handle {@link Employee} entity.
 */
public interface EmployeeRepository extends OrientRepository<Employee> {
	
	/**
	 * Searches and returns employees having given {@code firstName}.
	 * Executes query "select * from Employee where firstName = ?".
	 * 
	 * @param firstName {@link String} value of first name
	 * @return {@link List} of employees with given value as {@literal firstName}
	 */
	List<Employee> findByFirst(final String firstName);

	/**
	 * Searches and returns employees having given {@code lastName}.
	 * Executes query "select * from Employee where lastName = ?".
	 * 
	 * @param lastName {@link String} value of last name
	 * @return {@link List} of employees with given value as {@literal lastName}
	 */
	List<Employee> findByLast(final String lastName);

	/**
	 * Searches and return employees with given {@code firstName} and {@code lastName}.
	 * Executes query "select * from Employee where firstName = ? and lastName = ?".
	 * 
	 * @param firstName value of {@code firstName} in {@link Employee}
	 * @param lastName value of {@code lastName} in {@link Employee}
	 * @return {@link List} of employees with given {@literal firstName} and {@literal lastName}
	 */
	List<Employee> findByFirstAndLast(final String firstName, final String lastName);

	/**
	 * Searches and return employees with given {@code firstName} or {@code email}.
	 * Executes query "select * from Employee where firstName = ? or emailId = ?".
	 *
	 * @param firstName value of {@code firstName} in {@link Employee}
	 * @param emailId value of {@code emailId} in {@link Employee}
	 * @return {@link List} of employees with given firstName or emailId
	 */
	List<Employee> findEmployeeByFirstOrEmail(final String firstName, final String emailId);

	/**
	 * Since emailId is a unique field, it returns a matched employee
	 *
	 * @param emailId value of {@code emailId} of the {@link Employee}
	 * @return {@link Employee} with the given emailId
	 */
	Employee findEmployeeByEmail(final String emailId);

	/**
	 * Since id is a unique field, it returns a matched employee
	 *
	 * @param id value of {@code id} of the {@link Employee}
	 * @return {@link Employee} with the given id
	 */
	Employee findEmployeeById(final Long id);
	
	/**
	 * Since id is a unique field, it returns matched employees with given collection of id.
	 *
	 * @param ids collection of id value of {@code id} of the {@link Employee}
	 * @return list of {@link Employee} matched with the given collection of id
	 */
	List<Employee> findEmployeeByIdIn(final Collection<Long> ids);

	/**
	 * @param firstName string value
	 * @return employees whose first name starts with given value.
	 */
	List<Employee> findEmployeeByFirstStartingWith(final String firstName);

	/**
	 * Searches for employees whose name start with give string value.
	 * Executes query "select * from Employee where firstName like 'employee%'"
	 * 
	 * @param firstName string value
	 * @return employees whose first name is similar to the given value.
	 */
	List<Employee> findEmployeeByFirstLike(final String firstName);

	/**
	 * Searches for employees whose name does not start with give string value.
	 * Executes query "select * from Employee where not firstName like 'employee%'"
	 * 
	 * @param firstName string value
	 * @return employees whose first name is not similar to the given value.
	 */
	List<Employee> findEmployeeByFirstNotLike(final String firstName);

	/**
	 * Searches for employees whose name does not start with give string value.
	 * Executes query "select * from Employee where firstName is not null"
	 * 
	 * @return employees whose first name is not similar to the given value.
	 */
	List<Employee> findEmployeeByFirstIsNotNull();

	/**
	 * @param emailId string value of email id
	 * @return employees whose email id is equal to given value.
	 */
	@Query("select * from Employee where emailId.toLowerCase() = ?.toLowerCase()")
	Employee findEmployeByEmailIgnoreCase(final String emailId);

	/**
	 * Executes query "select * from Employee where not age = 25".
	 * 
	 * @param age integer value
	 * @return employees whose age is not equal to the given value.
	 */
	List<Employee> findEmployeeByUserAgeNot(final int age);
	
	/**
	 * Executes query "select * from Employee where age = 25".
	 * 
	 * @param age integer value
	 * @return employees whose age is equal to the given value.
	 */
	List<Employee> findEmployeeByUserAge(final int age);

	/**
	 * Executes query "select * from Employee where age > 25"
	 * 
	 * @param age integer value
	 * @return employees whose age is greater than the given value.
	 */
	List<Employee> findEmployeeByUserAgeGreaterThan(final int age);

	/**
	 * Executes query "select * from Employee where age >= 25"
	 * 
	 * @param age integer value
	 * @return employees whose age is greater than or equals to the given value.
	 */
	List<Employee> findEmployeeByUserAgeGreaterThanEqual(final int age);

	/**
	 * Executes query "select * from Employee where age < 25"
	 * 
	 * @param age integer value
	 * @return employees whose age is less than the given value.
	 */
	List<Employee> findEmployeeByUserAgeLessThan(final int age);

	/**
	 * Executes query "select * from Employee where age <= 25"
	 * 
	 * @param age integer value
	 * @return employees whose age is lesser than or equals to the given value.
	 */
	List<Employee> findEmployeeByUserAgeLessThanEqual(final int age);

	/**
	 * Executes query "select * from Employee where age is null"
	 * 
	 * @return employees whose age is not initialized.
	 */
	List<Employee> findEmployeeByUserAgeNull();

	/**
	 * @param age collection of integer values
	 * @return employees whose age falls in the given collection
	 */
	@Query(value = "select * from Employee where age in ?")
	List<Employee> findEmployeeByAgeIn(final Collection<Integer> age);

	/**
	 * Executes query "select * from Employee where isActive = 1"
	 * 
	 * @return employees whose active value is set true.
	 * getIsEmployeeActive
	 */
	List<Employee> findEmployeeByIsEmployeeActiveTrue();

	/**
	 * Executes query "select * from Employee where isActive = 0"
	 * 
	 * @return employees whose active value is set false.
	 */
	List<Employee> findEmployeeByIsEmployeeActiveFalse();
	
	/**
	 * Executes query "select * from Employee where  designation = 'CONSULTANT'".
	 * 
	 * @param designation enum value
	 * @return employees whose designation attribute matches the given value.
	 */
	List<Employee> findEmployeeByEmpDesignation(final Designation designation);

	/**
	 * Executes query "select * from Employee where  designation = 'CONSULTANT'".
	 * 
	 * @param designations collection of enum value
	 * @return employees whose designation attribute matches the given value.
	 */
	List<Employee> findEmployeeByEmpDesignationIn(final Collection<Designation> designations);

	/**
	 * Executes query "select count(*) from Employee where isActive = 1".
	 * 
	 * @param active boolean value for isActive field
	 * @return Returns the total of count matching the value.
	 */
	Long countByIsEmployeeActive(final Boolean active);

	/**
	 * Executes query eg. "select count(*) from Employee where designation = 'CONSULTANT' and isActive = 1 ".
	 * 
	 * @param designation enum value
	 * @param active boolean value
	 * @return count of employees whose designation and is active status match the given values
	 */
	Long countByEmpDesignationAndIsEmployeeActive(final Designation designation, final Boolean active);

	/**
	 * 
	 * Executes query such as "delete vertex from Employee where firstName = 'user1' ".
	 *
	 * @param firstName value of {@code firstName} in {@link Employee}
	 * @return number of entities deleted in database
	 */
	Long deleteByFirst(final String firstName);

	/**
	 * Executes query such as "select * from Employee where designation = 'CONSULTANT' limit 3 offset 24".
	 *
	 * @param designation value of {@code designation} in{@link Employee}
	 * @param pageRequest a pageable value
	 * @return a page of employee records.
	 */
	Page<Employee> findEmployeeByEmpDesignation(final Designation designation, final Pageable pageRequest);

	/**
	 * 
	 * Executes query such as "select * from Employee where designation = 'CONSULTANT' order by age asc"
	 *
	 * @param designation enum type of {@link Designation}
	 * @return employees whose designation matches the given value and are ordered by there age
	 */
	List<Employee> findEmployeeByEmpDesignationOrderByUserAgeAsc(final Designation designation);

	/**
	 * 
	 * Executes query such as "select * from Employee where designation = 'CONSULTANT' order by age asc limit 3 offset 0"
	 *
	 * @param designation enum type of {@link Designation}
	 * @param pageRequest a pageable value
	 * @return a page of employees whose designation matches the given value and are ordered by there age
	 */
	Page<Employee> findEmployeeByEmpDesignationOrderByUserAgeAsc(final Designation designation, final Pageable pageRequest);

	/**
	 * Executes the native query mentioned in as value in @Query annotation
	 *
	 * @param firstName name of the employee
	 * @param active boolean value to indication active employee
	 * @return total count of employees matching the given criteria
	 */
	@Query(value = "select count(*) from employee where firstName = ? and isActive = ?", count = true)
	Long countByFirstNameAndActive(final String firstName, final Boolean active);

	/**
	 * Searches for employees whose name does not start with give string value.
	 * Executes query "select * from Employee where not firstName = 'emp29'"
	 * 
	 * @param firstName string value
	 * @return employees whose first name is not equal to the given value.
	 */
	List<Employee> findEmployeeByFirstNot(final String firstName);

	/**
	 * Executes the native query mentioned in as value in @Query annotation
	 * 
	 * @param firstName string value
	 * @return employees whose first name substring matches to the given value.
	 */
	@Query(value = "SELECT FROM Employee WHERE firstName.substring(0,3) = ?")
	List<Employee> findEmployeeByFirst(final String firstName);

	/**
	 * Executes the native query mentioned in as value in @Query annotation
	 * 
	 * @param firstName string value
	 * @return returns number of employees deleted
	 */
	@Query(value = "DELETE vertex FROM Employee WHERE firstName.toUpperCase() = ?")
	Long deleteEmployeeByFirstName(final String firstName);
	
	/**
	 * Executes query such as "select * from Employee where designation = 'CONSULTANT' limit 3 offset 24".
	 *
	 * @param firstName string value in{@link Employee}
	 * @param pageRequest a pageable value
	 * @return a page of employee records.
	 */
	Page<Employee> findEmployeeByFirst(final String firstName, final Pageable pageRequest);


}
