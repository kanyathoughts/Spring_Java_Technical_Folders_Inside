package com.lovetocode.example.cruddemo;

import java.util.List;

import org.hibernate.internal.build.AllowSysOut;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import com.lovetocode.example.cruddemo.dao.AppDAO;
import com.lovetocode.example.cruddemo.entity.Course;
import com.lovetocode.example.cruddemo.entity.Instructor;
import com.lovetocode.example.cruddemo.entity.InstructorDetail;

@SpringBootApplication
public class CruddemoApplication {

	public static void main(String[] args) {
		SpringApplication.run(CruddemoApplication.class, args);
	}
	
	@Bean
	public CommandLineRunner commandLineRunner(AppDAO appDAO) {
		return runner -> {
			//createInstructor(appDAO);
			
			//finding instructor which retrieves instructor details table as well as by default OnetoOne fetching pattern is eager
			//which means it will fetch all the associated table details as well
//			Instructor tempInstructor = appDAO.findInstructorById(100);
//			System.out.println(tempInstructor);
//			System.out.println(tempInstructor.getInstructorDetail());
			
			//delete instructor
			//appDAO.deleteInstructorById(101);
			
			//It will fetch the associated instructor entity also as we have set up the bi-directional connection
			//findInstructorDetail(appDAO);
			
			
			//It will delete the associated instructor entity also
//			deleteInstructorDetailById(appDAO);
			
			//Add instructor-detail of the specific instructor
			
//			InstructorDetail tempDetail = new InstructorDetail("youtube.com", "watching youtube");
//			Instructor tempInstructor = appDAO.findInstructorById(100);
//			tempInstructor.setInstructorDetail(tempDetail);
//			appDAO.save(tempInstructor);
			
			// add instructor with courses
			//createInstructorWithCourses(appDAO);
			
			//findInstructorWithCourses(appDAO);
			
			//findCoursesForInstructor(appDAO);
			
			//findInstructorWithCoursesJoinFetch(appDAO);
			
			//updateInstructor(appDAO);
			
			//updateCourse(appDAO);
			
			//deleteInstructorByIdAndDoNotRemoveCourses(appDAO);
			
			deleteCourse(appDAO);
			
		};
	}

	private void deleteCourse(AppDAO appDAO) {
		int id = 10;
		
		System.out.println("deleting course with id: " + id);
		
		appDAO.deleteCourseById(id);
		
		System.out.println("Done!!");
	}

	private void deleteInstructorByIdAndDoNotRemoveCourses(AppDAO appDAO) {
		int id = 1;
		
		appDAO.deleteInstructorByIdDoNotRemoveCourse(id);
	}

	private void updateCourse(AppDAO appDAO) {
		int id = 10;
		Course tempCourse = appDAO.findCourseById(id);
		
		tempCourse.setTitle("This is helicopter");
		
		appDAO.update(tempCourse);
	
		System.out.println("Done!!");
	}

	private void updateInstructor(AppDAO appDAO) {
		int id = 1;
		
		Instructor tempInstructor = appDAO.findInstructorById(id);
		
		System.out.println("Instructor before: " + tempInstructor);
		
		tempInstructor.setLastName("Jashu");
		
		appDAO.save(tempInstructor);
		
		System.out.println("Instructor after: " + tempInstructor);
		
	}

	private void findInstructorWithCoursesJoinFetch(AppDAO appDAO) {
		int id = 1;
		
		Instructor tempInstructor = appDAO.findInstructorByIdJoinFetch(id);
		
		System.out.println("instructor is: " + tempInstructor);
		
		tempInstructor.getCourses().forEach(System.out::println);
		
		
	}

	private void findCoursesForInstructor(AppDAO appDAO) {
		int id = 1;
		
		Instructor tempInstructor = appDAO.findInstructorById(id);
		
		List<Course> courses = appDAO.findCoursesByInstructorId(id);
		
//		courses.forEach(System.out::println);
		
		//here if we don't set the courses it will throw Lazyfetch Exception 
		//if we set the courses since courses have been set it can easily find them
		tempInstructor.setCourses(courses);
		
		tempInstructor.getCourses().forEach(System.out::println);
		
	}

	private void findInstructorWithCourses(AppDAO appDAO) {
		Instructor tempInstructor = appDAO.findInstructorById(1);
		
		tempInstructor.getCourses().forEach(System.out::println);
	}

	private void createInstructorWithCourses(AppDAO appDAO) {
		//Create instructor object
		Instructor tempInstructor = new Instructor("Jashu", "Manigandla", "jashu@gmail.com");
				
		//Create instructor details object
		InstructorDetail tempInstructorDetail = new InstructorDetail("http://www.youtube.com", "Watching youtube");
				
		//associate objects
		tempInstructor.setInstructorDetail(tempInstructorDetail);
		
		//create some courses
		Course tempCourse1 = new Course("The badminton");
		Course tempCourse2 = new Course("The Football");
		tempInstructor.add(tempCourse1);
		tempInstructor.add(tempCourse2);
		
		//saving the instructor
		
		System.out.println("saving instructor: " + tempInstructor);
		System.out.println("saving courses: " + tempInstructor.getCourses());
		
		appDAO.save(tempInstructor);
		
		System.out.println("Done!!!");
		
	}

	private void deleteInstructorDetailById(AppDAO appDAO) {
		int id = 4;
		
		appDAO.deleteInstructorDetailById(id);
		
		System.out.println("Done!!");
		
	}

	private void findInstructorDetail(AppDAO appDAO) {
		//get the instructor detail object
		InstructorDetail tempInstructor = appDAO.findInstructorDetailById(1);
		
		//print the instructor detail
		System.out.println(tempInstructor);
		
		//print the associated instructor
		System.out.println(tempInstructor.getInstructor());
		
		System.out.println("Done!!");
	}

	private void createInstructor(AppDAO appDAO) {
		//Create instructor object
		Instructor tempInstructor = new Instructor("Jashu", "Manigandla", "jashu@gmail.com");
		
		//Create instructor details object
		InstructorDetail tempInstructorDetail = new InstructorDetail("http://www.youtube.com", "Watching youtube");
		
		//associate objects
		tempInstructor.setInstructorDetail(tempInstructorDetail);
		
		System.out.println("saving : " + tempInstructor);
		
		//saving the instructor
		appDAO.save(tempInstructor);
		
		System.out.println("Done");
		
	}

}
