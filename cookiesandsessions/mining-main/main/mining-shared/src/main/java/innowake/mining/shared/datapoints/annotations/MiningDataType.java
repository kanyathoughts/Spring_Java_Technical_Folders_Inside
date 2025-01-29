/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;

/**
 * Defines meta data for a {@link MiningDataTypeDefinition}.
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface MiningDataType {
	
	/**
	 * Name for the data type.
	 */
	public String name() default "";
}
