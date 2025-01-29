package com.springSec.example.springSecurityDemo.Controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.springSec.example.springSecurityDemo.model.Student;

import jakarta.servlet.http.HttpServletRequest;

@RestController
public class StudentController {

    List<Student> students = new ArrayList<>(Arrays.asList(
            new Student(1, "kanya", 45),
            new Student(2, "akhil", 55)));

    @GetMapping("/students")
    public List<Student> getStudents() {
        return students;
    }

    // get csrf token
    // Using this token you can add X-CSRF-TOKEN header to the post request and
    // token as the value then post request will be success otherwise it will fail
    // because for post, put and delete requests csrf token is mandatory
    @GetMapping("/csrf")
    public CsrfToken getcsrfToken(HttpServletRequest request) {
        return (CsrfToken) request.getAttribute("_csrf");
    }

    @PostMapping("/students")
    public Student addStudent(@RequestBody Student student) {
        students.add(student);
        return student;
    }
}
