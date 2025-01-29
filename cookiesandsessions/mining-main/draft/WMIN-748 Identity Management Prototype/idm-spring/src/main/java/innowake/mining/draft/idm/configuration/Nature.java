/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.draft.idm.configuration;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.security.access.annotation.Secured;

@Target({ ElementType.METHOD, ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Secured({ "this-value-is-totally-irrelevant-but-cannot-be-empty" })
public @interface Nature {

	enum Type {
		DISCOVREY_LIGHT,
		DISCOVERY,
		MINING
	}

	public Type[] value();
	
	public String projectId() default "#projectId";
}
