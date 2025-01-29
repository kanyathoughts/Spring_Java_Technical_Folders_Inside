/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import org.springframework.security.access.annotation.Secured;
import org.springframework.web.bind.annotation.RestController;

/**
 * Drop-in replacement annotation for {@link RestController @RestController} with the
 * addition of also having a {@link Secured @Secured} annotation associated.
 * <p>
 * This annotation is mandatory for all REST endpoints in the Mining API server.
 */
@Retention(RUNTIME)
@Target(TYPE)
@RestController
@Secured("irrelevant-value")
public @interface MiningRestController {

}
