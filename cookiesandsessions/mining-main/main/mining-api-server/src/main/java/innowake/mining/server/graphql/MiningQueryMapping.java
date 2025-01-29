/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.core.annotation.AliasFor;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.security.access.annotation.Secured;

/**
 * Annotation for mining GraphQl query methods with added authorization.
 */
@Target(value = {ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@SchemaMapping(typeName = "Query")
@Secured("irrelevant-value")
public @interface MiningQueryMapping {

	@AliasFor(annotation = SchemaMapping.class, attribute = "field")
	String name() default "";

	@AliasFor(annotation = SchemaMapping.class, attribute = "field")
	String value() default "";
}
