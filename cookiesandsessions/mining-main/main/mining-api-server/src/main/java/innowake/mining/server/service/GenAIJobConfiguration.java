/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.properties.GenericConfigProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


@Configuration
public class GenAIJobConfiguration {

	@Autowired
	GenericConfigProperties genericConfigProperties;


	@Bean
	public MonitoredTaskManagerService monitoredTaskManagerServiceCreate() {
	  return new MonitoredTaskManagerService(genericConfigProperties);
	}

}
