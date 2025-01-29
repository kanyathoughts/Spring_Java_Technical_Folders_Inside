package com.example.unitTesting.UnitTesting.controller;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.RequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@WebMvcTest(HelloWorldController.class)
public class HelloWorldControllerTest {
	
	@Autowired
	private MockMvc mockMVC;
	
	@Test
	public void helloWorldTest() throws Exception {
		RequestBuilder request = MockMvcRequestBuilders
				.get("/hello-world")
				.accept(MediaType.APPLICATION_JSON);
		MvcResult result = mockMVC.perform(request).andReturn();
		assertEquals("Hello-world", result.getResponse().getContentAsString());
		
		
		// Here status and content methods are part of MockMVCResponseMatchers class which we have done
		// static import so without using class name we are using these methods
		MvcResult result2 = mockMVC.perform(request)
									.andExpect(status().isOk())
									.andExpect(content().string("Hello-world"))
									.andReturn();
		assertEquals("Hello-world", result2.getResponse().getContentAsString());
		
	}

}
