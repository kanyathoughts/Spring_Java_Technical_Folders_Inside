package com.springSec.example.springSecurityDemo.SecurityConfig;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cglib.proxy.NoOp;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.NoOpPasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
public class SecurityConfigFile {

    @Autowired
    private UserDetailsService userDetailsService;

    @Autowired
    private JWTFilter jwtFilter;

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {

        // disabling csrf means client doesn't get csrf token
        http.csrf(customizer -> customizer.disable());
        // authorizing all the the requests except register and login apis
        http.authorizeHttpRequests(request -> request
                .requestMatchers("/register", "/userLogin")
                .permitAll()
                .anyRequest().authenticated());
        // Enabling form login in browser
        http.formLogin(Customizer.withDefaults());
        // Enabling basic auth with default security filter chain (Postman)
        http.httpBasic(Customizer.withDefaults());
        // making http stateless
        // http.sessionManagement(session ->
        // session.sessionCreationPolicy(SessionCreationPolicy.STATELESS));
        http.addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class);
        return http.build();

    }

    // In this way we can add users to application and if you can see the spring
    // security architecture this layer will provide the userDetails object to
    // Authentication provider layer that will send authentication object to
    // security filter but here we are directly setting up the users without using
    // the external databases but if you want to use external database then you have
    // to work with Authentication provider layer which actually connects with
    // database for getting username and password and performs the password encoding
    // on the password received from the user
    //

    //
    // @Bean
    // public UserDetailsService userDetailsService() {
    // UserDetails user1 = User
    // .withDefaultPasswordEncoder()
    // .username("akhil")
    // .password("akhil")
    // .roles("USER")
    // .build();

    // UserDetails user2 = User
    // .withDefaultPasswordEncoder()
    // .username("sai")
    // .password("sai")
    // .roles("USER")
    // .build();

    // return new InMemoryUserDetailsManager(user1, user2);
    // }

    @Bean
    public AuthenticationProvider authenticationProvider() {
        DaoAuthenticationProvider provider = new DaoAuthenticationProvider();
        // Using bcrypt algorithm
        provider.setPasswordEncoder(new BCryptPasswordEncoder(12));
        // provider.setPasswordEncoder(NoOpPasswordEncoder.getInstance());
        provider.setUserDetailsService(userDetailsService);
        return provider;
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration configure) throws Exception {
        return configure.getAuthenticationManager();
    }

}
