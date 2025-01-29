/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import org.springframework.web.bind.annotation.RestController;

/**
 * Drop-in replacement annotation for {@link RestController @RestController} for explicitly
 * declaring REST controllers, which are not authenticated.
 */
@Retention(RUNTIME)
@Target(TYPE)
@RestController
public @interface MiningUnsecuredRestController {

}
