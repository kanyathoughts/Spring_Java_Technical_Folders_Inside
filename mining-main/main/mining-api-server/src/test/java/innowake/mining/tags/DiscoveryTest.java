/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.tags;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.junit.jupiter.api.Tag;

/**
 * Annotation to add on test classes which will be needed to create Discovery build
 */
@Target({ ElementType.TYPE, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
@Tag("Discovery")
public @interface DiscoveryTest {

}
