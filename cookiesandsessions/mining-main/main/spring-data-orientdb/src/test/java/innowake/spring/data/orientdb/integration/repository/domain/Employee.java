/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.EdgeDirection;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.Property;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.Relationship;


/**
 * A vertex class mapped in orientDB.
 */
@JsonInclude(Include.NON_NULL)
@Entity
public class Employee implements Comparable<Employee> {

	@JsonIgnore
	@RId
	@Nullable
	private String rid;	
	@JsonIgnore
	@Id(sequence = "Employee_Sequence")
	@Nullable
	private Long id;	
	@Property( "firstName")
	@Nullable
	private String first;	
	@Property("lastName")
	@Nullable
	private String last;	
	@Property("emailId")
	@Nullable
	private String email;	
	@Property( "age")
	@Nullable
	private Integer userAge;	
	@Property( "isActive")
	@Nullable
	private Boolean isEmployeeActive;	
	@Property( "secretCode")
	@Nullable
	private byte[] secretEmpCode;

	@Relationship(name = "HasReportsTo", direction = EdgeDirection.BOTH)
	@JsonIgnore
	@Nullable
	private HasReportsTo reportsTo;

	@Relationship(name = "Employee_projects", direction = EdgeDirection.OUT)
	@JsonIgnoreProperties("out")
	@Nullable
	private List<Employee_projects> projects;

	@Relationship(name = "Employee_coaches", direction = EdgeDirection.BOTH)
	@JsonIgnore
	@Nullable
	private List<Employee_coaches> coaches;
	
	@Property("skills")
	@Nullable
	private List<String> empSkills;

	@Relationship(name = "Employee_trainingStatus", direction = EdgeDirection.OUT)
	@Nullable
	private Map<String, String> trainingStatus;
	
	@Property( "designation")
	@Nullable
	private Designation empDesignation;	
	@Property("access")
	@Nullable
	private List<Access> empAccess;

	@Relationship(name = "Employee_projectStatus", direction = EdgeDirection.OUT)
	@JsonIgnore
	@Nullable
	private Map<String, Employee_projectStatus> projectStatus;
	
	@Relationship(name = "Employee_projectStatus", direction = EdgeDirection.OUT)
	@JsonIgnore
	@Nullable
	private Map<String, Project> projectsMap;
	
	/* Not supported currently */
	@Relationship(name = "Employee_subProjects", direction = EdgeDirection.OUT)
	@Nullable
	private Map<Project, Project> subProjects;

	/**
	 * Default constructor required to create employee proxy instance.
	 */
	public Employee() {
	}

	/**
	 * Initialize employee class with given values.
	 * 
	 * @param firstName first name of employee
	 * @param lastName last name of employee
	 * @param emailId unique email id of the employee
	 */
	public Employee(final String firstName, final String lastName, final String emailId) {
		this.first = firstName;
		this.last = lastName;
		this.email = emailId;
	}

	/**
	 * Initialize employee class with given values.
	 * 
	 * @param firstName first name of employee
	 * @param lastName last name of employee
	 * @param emailId unique email id of the employee
	 * @param employee instance of {@link Employee}
	 */
	public Employee(final String firstName, final String lastName, final String emailId, final Employee employee) {
		this.first = firstName;
		this.last = lastName;
		this.email = emailId;
		this.reportsTo = new HasReportsTo(employee, this);
	}

	/**
	 * Initialize employee class with given values.
	 * 
	 * @param firstName first name of employee
	 * @param lastName last name of employee
	 * @param emailId unique email id of the employee
	 * @param projects collection of projects
	 */
	public Employee(final String firstName, final String lastName, final String emailId, final List<Project> projects) {
		this.first = firstName;
		this.last = lastName;
		this.email = emailId;
		this.setProjects(projects.stream().map(project -> new Employee_projects(this, project)).collect(Collectors.toList()));
	}

	/**
	 * @return Returns record id.
	 */
	@Nullable
	public String getRid() {
		return rid;
	}

	/**
	 * Sets the record id field.
	 *
	 * @param rid id to be set
	 */
	public void setRid(final String rid) {
		this.rid = rid;
	}

	/**
	 * Returns the id value.
	 * 
	 * @return the id
	 */
	@Nullable
	public Long getId() {
		return id;
	}

	/**
	 * Sets the id field.
	 *
	 * @param id the id to be set
	 */
	public void setId(final Long id) {
		this.id = id;
	}

	/**
	 * @return Returns first name of the employee.
	 */
	@Nullable
	public String getFirst() {
		return first;
	}

	/**
	 * Sets employee's firstName.
	 *
	 * @param firstName {@link String} name
	 */
	
	public void setFirst(final String firstName) {
		this.first = firstName;
	}

	/**
	 * @return Returns last name of the employee.
	 */
	@Nullable
	public String getLast() {
		return last;
	}

	/**
	 * Sets employee's lastName.
	 *
	 * @param lastName {@link String} name
	 */
	public void setLast(final String lastName) {
		this.last = lastName;
	}

	/**
	 * @return Returns email id of the employee.
	 */
	@Nullable
	public String getEmail() {
		return email;
	}

	/**
	 * Sets employee's emailId.
	 *
	 * @param emailId {@link String} emailId, should be unique
	 */
	public void setEmail(final String emailId) {
		this.email = emailId;
	}

	/**
	 * @return Returns age of the employee.
	 */
	@Nullable
	public Integer getUserAge() {
		return userAge;
	}

	/**
	 * Sets employee's age.
	 *
	 * @param age {@link Integer} age
	 */
	public void setUserAge(final Integer age) {
		this.userAge = age;
	}

	/**
	 * @return Returns true if the employee is active.
	 */
	@Nullable
	public Boolean getIsEmployeeActive() {
		return isEmployeeActive;
	}

	/**
	 * Sets employee's active status.
	 *
	 * @param isActive {@link Boolean} true or false
	 */
	public void setIsEmployeeActive(final Boolean isActive) {
		this.isEmployeeActive = isActive;
	}

	/**
	 * @return Returns collection of string, if not null.
	 */
	@Nullable
	public List<String> getEmpSkills() {
		return empSkills;
	}

	/**
	 * Sets employee's skills.
	 *
	 * @param skills {@link List} of {@link String}
	 */
	public void setEmpSkills(final List<String> skills) {
		this.empSkills = skills;
	}

	/**
	 * @return Returns a map of strings, if not null.
	 */
	@Nullable
	public Map<String, String> getTrainingStatus() {
		return trainingStatus;
	}

	/**
	 * Sets employee's traingStatus.
	 *
	 * @param trainingStatus {@link Map} of string values
	 */
	public void setTrainingStatus(final Map<String, String> trainingStatus) {
		this.trainingStatus = trainingStatus;
	}

	/**
	 * @return Returns a {@link Designation} type, if not null.
	 */
	@Nullable
	public Designation getEmpDesignation() {
		return empDesignation;
	}

	/**
	 * Sets employee's designation.
	 *
	 * @param designation {@link Designation} enum type
	 */
	public void setEmpDesignation(final Designation designation) {
		this.empDesignation = designation;
	}

	/**
	 * @return Returns a collection of enum {@link Access} type, if not null.
	 */
	@Nullable
	public List<Access> getEmpAccess() {
		return empAccess;
	}

	/**
	 * Sets employee's accesses.
	 *
	 * @param access {@link List} of {@link Access} type
	 */
	public void setEmpAccess(final List<Access> access) {
		this.empAccess = access;
	}

	/**
	 * @return Returns a map of {@link String} with value as {@link Employee_projectStatus}
	 */
	@Nullable
	public Map<String, Employee_projectStatus> getProjectStatus() {
		return projectStatus;
	}

	/**
	 * Sets employee's projectStatus.
	 *
	 * @param projectStatus {@link Map} of projects with string
	 */
	public void setProjectStatus(final Map<String, Employee_projectStatus> projectStatus) {
		this.projectStatus = projectStatus;
	}

	
	
	/**
	 * @return Returns a map of {@link String} with value as {@link Project}
	 */
	@Nullable
	public Map<String, Project> getProjectsMap() {
		return projectsMap;
	}

	
	/**
	 * Sets employee's projectStatus.
	 *
	 * @param projectsMap {@link Map} of projects with string
	 */
	public void setProjectsMap(final Map<String, Project> projectsMap) {
		this.projectsMap = projectsMap;
	}

	/**
	 * Returns {@link HasReportsTo}.
	 *
	 * @return Returns {@link HasReportsTo}
	 */
	public @Nullable HasReportsTo getReportsTo() {
		return reportsTo;
	}

	
	/**
	 * Sets {@link HasReportsTo}.
	 *
	 * @param reportsTo {@link HasReportsTo}
	 */
	public void setReportsTo(final @Nullable HasReportsTo reportsTo) {
		this.reportsTo = reportsTo;
	}

	
	/**
	 * Returns {@link Employee_projects}.
	 *
	 * @return {@link Employee_projects}
	 */
	public @Nullable List<Employee_projects> getProjects() {
		return projects;
	}

	
	/**
	 * Sets {@link Employee_projects}.
	 *
	 * @param projects list of {@link Employee_projects}
	 */
	public void setProjects(final List<Employee_projects> projects) {
		this.projects = projects;
	}

	
	/**
	 * Returns {@link Employee_coaches}
	 *
	 * @return Employee_coaches
	 */
	public @Nullable List<Employee_coaches> getCoaches() {
		return coaches;
	}

	
	/**
	 * Sets list of {@link Employee_coaches}
	 *
	 * @param coaches list of {@link Employee_coaches}
	 */
	public void setCoaches(final @Nullable List<Employee_coaches> coaches) {
		this.coaches = coaches;
	}

	/**
	 * @return Returns a map of {@link Project} with value as {@link Project}
	 */
	@Nullable
	public Map<Project, Project> getSubProjects() {
		return subProjects;
	}

	/**
	 * Sets employee's subProjects.
	 *
	 * @param subProjects {@link Map} of projects
	 */
	public void setSubProjects(final Map<Project, Project> subProjects) {
		this.subProjects = subProjects;
	}
	
	/**
	 * @return Returns secretCode.
	 */
	@Nullable
	public byte[] getSecretEmpCode() {
		return secretEmpCode;
	}

	
	/**
	 * Sets employee's secretCode.
	 *
	 * @param secretCode to be set
	 */
	public void setSecretEmpCode(byte[] secretCode) {
		this.secretEmpCode = secretCode;
	}

	@Override
	public String toString() {
		return "Employee [rid=" + rid + ", id=" + id + ", firstName=" + first + ", lastName=" + last + ", emailId=" + email 
				+ " ]";
	}

	/**
	 * An enum class for employee's designation.
	 */
	@Entity
	public enum Designation {
		
		
		/**
		 * CONSULTANT.
		 */
		CONSULTANT,
		/**
		 * MANAGER.
		 */
		MANAGER;
		
		/**
		 * Returns the String representation of the enum.
		 *
		 * @param designation the designation value
		 * @return the String representation of the enum
		 */
		public static String getDesignation(final Designation designation) {
			switch (designation) {
				case CONSULTANT:
					return CONSULTANT.toString();
				case MANAGER:
					return MANAGER.toString();	
				default:
					throw new IllegalArgumentException("Unsupported enum value");
			}
		}
	}
	
	/**
	 * Returns the String representation of the enum.
	 *
	 * @param access the access type
	 * @return the string representation of the enum
	 */
	public static String getAccess(final Access access) {
		switch (access) {
			case ADMIN:
				return "admin";
			case USER:
				return "user";	
			case DEVELOPER:
				return "developer";	
			default:
				throw new IllegalArgumentException("Unsupported enum value");
		}
	}

	/**
	 * An enum class for employee's access.
	 */
	@Entity
	public enum Access {
		/**
		 * ADMIN.
		 */
		ADMIN,
		/**
		 * USER.
		 */
		USER,
		/**
		 * DEVELOPER.
		 */
		DEVELOPER,
		/**
		 * TESTER.
		 */
		TESTER
	}

	@Override
	public int compareTo(@Nullable final Employee employee) {
		if (email != null && employee != null) {
			return email.compareTo(employee.email);
		}
		return 0;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

}
