/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.permission;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * This configuration specifies the selection of the {@linkplain GenAIModulePermissionChecker} bean.
 */
@Configuration
public class PermissionConfig {
	
	/**
	 * This method is used to provide the {@linkplain GenAIModulePermissionChecker} Bean as long as no other Bean
	 * of type {@linkplain GenAIModulePermissionChecker} is defined.
	 * 
	 * @return instance of {@linkplain GenAIModulePermissionChecker}
	 */
	@ConditionalOnProperty(name = "configuration.gen-ai-require-filtering", havingValue = "false")
	@ConditionalOnMissingBean
	@Bean
	public GenAIModulePermissionChecker genAIModulePermissionChecker() {
		return new GenAIModulePermissionChecker() {};
	}

}
