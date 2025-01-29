package com.springSec.example.springSecurityDemo.Controller;

import org.springframework.web.bind.annotation.RestController;

import jakarta.servlet.http.HttpServletRequest;

import org.springframework.web.bind.annotation.GetMapping;

@RestController
public class HelloController {

    // Every controller is a servlet in spring boot
    // These all servlets will be running inside servlet container which is tomcat
    @GetMapping("/")
    public String greeting(HttpServletRequest request) {
        return "Hello-world\n" + request.getSession().getId();
    }
}
