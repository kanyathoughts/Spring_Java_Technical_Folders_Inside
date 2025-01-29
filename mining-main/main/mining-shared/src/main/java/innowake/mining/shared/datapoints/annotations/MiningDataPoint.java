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
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;

/**
 * Defines meta data for a {@link MiningDataPointDefinition}.
 */
@Repeatable(MiningDataPoints.class)
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface MiningDataPoint {

	/**
	 * Name of the data point. Defaults to the name of the annotated property.
	 */
	String name() default "";

	/**
	 * Type of the data point. Defaults to the type of the annotated property.
	 * Use only one of {@link #scalarType()}, {@link #referenceTypeName()} or {@link #type()}.
 	 */
	ScalarType scalarType() default ScalarType.UNDEFINED;

	/**
	 * Type of the data point. Defaults to the type of the annotated property.
	 * Use only one of {@link #scalarType()}, {@link #referenceTypeName()} or {@link #type()}.
	 */
	String referenceTypeName() default "";

	/**
	 * Type of the data point. Defaults to the type of the annotated property.
	 * Use only one of {@link #scalarType()}, {@link #referenceTypeName()} or {@link #type()}.
	 */
	Class<?> type() default void.class;

	/**
	 * Name of the data point to be displayed on the UI.
	 */
	String displayName() default "";
	
	/**
	 * Description of the data point to be displayed on the UI.
	 */
	String description() default "";
}
