/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.annotation;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import org.springframework.cache.annotation.Cacheable;

/**
 * Annotation for the method parameter containing the projectId.
 * This is used on methods annotated with {@link Cacheable} in order to resolve a project-specific Cache.
 */
@Retention(RUNTIME)
@Target(ElementType.PARAMETER)
public @interface ProjectIdArgument {
}
