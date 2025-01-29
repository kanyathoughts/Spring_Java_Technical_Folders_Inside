package com.springSec.example.springSecurityDemo.Controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.springSec.example.springSecurityDemo.model.Users;
import com.springSec.example.springSecurityDemo.service.UserService;

@RestController
public class UserController {

    @Autowired
    private UserService userService;

    @PostMapping("/register")
    public Users registerUser(@RequestBody Users user) {
        return userService.register(user);
    }

    @PostMapping("/userLogin")
    public String login(@RequestBody Users user) {
        return userService.verify(user);
    }

}
