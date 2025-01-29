/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import innowake.mining.shared.ProjectLockCategory;

/**
 * Annotation for locking the user interaction methods
 */
@Retention(RUNTIME)
@Target(METHOD)
public @interface TryLock {

	/**
	 * Specifies the lock category for the annotated method.
	 * @return the lock category for the annotated method.
	 */
	public ProjectLockCategory lockCategory();

	/**
	 * Provides a reason phrase or description for using the lock.This can help in understanding the purpose of the lock.
	 * @return the reason phrase or description for using the lock.
	 */
	public String reasonPhrase();

}
