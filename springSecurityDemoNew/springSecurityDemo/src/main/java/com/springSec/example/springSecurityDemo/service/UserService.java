package com.springSec.example.springSecurityDemo.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import com.springSec.example.springSecurityDemo.Repo.UserRepo;
import com.springSec.example.springSecurityDemo.model.Users;

@Service
public class UserService {

    @Autowired
    private UserRepo userRepo;

    @Autowired
    AuthenticationManager authenticationManager;

    @Autowired
    JWTService jwtService;

    // We are using bcrypt hashing for encoding the password
    BCryptPasswordEncoder encoder = new BCryptPasswordEncoder(12);

    public Users register(Users user) {
        user.setPassword(encoder.encode(user.getPassword()));
        return userRepo.save(user);
    }

    // Here everything we have already provided in the authentication provider
    // Here we are just handling the login part
    // if user is authenticated then show me the success message.
    public String verify(Users user) {
        // Here this will tell us entire internal architecture flow
        // Authentication manager takes the UsernamePasswordAuthenticationToken and it
        // will call the providers and providers will internally call data layer and
        // check whether user exists or not, after that Authentication object is sent
        // back to the Authentication manager which we are holding here and checking
        // whether it is authenticated or not.
        // if user is not present we were already throwing the UserNotFoundException in
        // MyUserDetailsService class

        Authentication authentication = authenticationManager
                .authenticate(new UsernamePasswordAuthenticationToken(user.getUsername(), user.getPassword()));

        if (authentication.isAuthenticated())
            return jwtService.generateToken(user.getUsername());

        return "failed";
    }

}
