/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;

/**
 * Defines a usage for a {@link MiningDataPointDefinition}.
 */
@Repeatable(MiningDataPointUsages.class)
@Target({ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Usage {
	
	/**
	 * Name of the data point to be displayed on the UI.
	 */
	String value() default "";
	
	UsageAttribute[] attributes() default {};
}
