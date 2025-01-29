/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;

/**
 * This code will be executed after the spring application startup has been finished.
 */
@Component
public class ApplicationStartupRunner implements ApplicationRunner {
	
	/* Injecting the hazelcast instance here is not possible, as this would lead to a dependency injection circle. */
	@Autowired
	private JobConfiguration jobConfig;

	@Override
	public void run(@Nullable final ApplicationArguments args) throws Exception {
		/* Start any pending background operations here through the JobConfiguration. Some background operations cannot be directly created during the
		 * initialization of JobConfiguration as it would lead to a dependency injection cycle. */
		jobConfig.startBackgroundOperations();
	}
}