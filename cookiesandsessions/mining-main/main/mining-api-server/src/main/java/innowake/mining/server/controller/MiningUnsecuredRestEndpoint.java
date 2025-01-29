/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Annotation for explicitly declaring REST endpoints as not secured.
 */
@Retention(RUNTIME)
@Target(METHOD)
public @interface MiningUnsecuredRestEndpoint {

}
