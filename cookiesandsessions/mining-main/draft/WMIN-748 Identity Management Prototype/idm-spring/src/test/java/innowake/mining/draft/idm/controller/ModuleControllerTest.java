/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.draft.idm.controller;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@RunWith(SpringRunner.class)
@WebMvcTest(value=ModuleController.class, excludeAutoConfiguration = { 
      SecurityAutoConfiguration.class 
  })
@ActiveProfiles(profiles = "test")
public class ModuleControllerTest {

	@Autowired
	private MockMvc mvc;

	@Test
	public void getPublicCall() throws Exception {
		mvc.perform( 
				MockMvcRequestBuilders.get("/api/public/projects/4711").accept(MediaType.APPLICATION_JSON)
			)
		.andExpect(
				MockMvcResultMatchers.status().isOk()
			);
	}
	
	@Test
	public void getProtectedCall() throws Exception {
		final String val = mvc.perform( 
				MockMvcRequestBuilders.get("/api/v1/projects/4711/discovery/admin").accept(MediaType.APPLICATION_JSON)
				)
		.andExpect(
				MockMvcResultMatchers.status().isOk()
				)
		.andReturn().getResponse().getContentAsString();
		System.out.println(val);
	}
}
