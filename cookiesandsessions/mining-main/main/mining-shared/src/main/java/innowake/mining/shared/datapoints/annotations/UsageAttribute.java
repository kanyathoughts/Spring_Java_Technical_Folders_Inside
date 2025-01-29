/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;

/**
 * Defines a usage attribute for a {@link MiningDataPointDefinition}. Use inside of a {@link Usage} annotation.
 */
@Target({ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface UsageAttribute {
	
	/**
	 * Key of the data point usage attribute.
	 */
	String key();
	
	/**
	 * Value of the data point usage attribute.
	 */
	String value();
}
