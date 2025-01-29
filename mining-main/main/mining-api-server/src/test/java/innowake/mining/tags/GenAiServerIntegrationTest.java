/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.tags;

import org.junit.jupiter.api.Tag;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to add on test classes which are relevant for GenAI related server integration tests
 */
@Target({ ElementType.TYPE, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
@Tag("GenAiServerIntegrationTest")
public @interface GenAiServerIntegrationTest {
}
