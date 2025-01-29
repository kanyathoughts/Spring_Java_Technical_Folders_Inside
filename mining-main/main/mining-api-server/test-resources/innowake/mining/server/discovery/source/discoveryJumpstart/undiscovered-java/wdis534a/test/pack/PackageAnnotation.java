/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package wdis534a.test.pack;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.CLASS)
@Target(ElementType.PACKAGE)
/**
 * Test annotation on package
 */
public @interface PackageAnnotation {
}
