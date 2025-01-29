package com.springSec.example.springSecurityDemo.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.springSec.example.springSecurityDemo.Exception.MyCustomException;
import com.springSec.example.springSecurityDemo.Repo.UserRepo;
import com.springSec.example.springSecurityDemo.model.UserPrinciple;
import com.springSec.example.springSecurityDemo.model.Users;

import jakarta.servlet.http.HttpServletRequest;

@Service
public class MyUserDetailsService implements UserDetailsService {

    @Autowired
    private UserRepo userRepo;

    @Override
    public UserDetails loadUserByUsername(String username) {

        Users user = userRepo.findByUsername(username);

        if (user == null) {
            // try {
            // throw new MyCustomException(404, username + " not found");
            // } catch (MyCustomException e) {
            // e.printStackTrace();
            // }
            throw new UsernameNotFoundException("User is not found");
        }

        // Here user will hold the user name and password so we can pass this here
        return new UserPrinciple(user);

    }

}
