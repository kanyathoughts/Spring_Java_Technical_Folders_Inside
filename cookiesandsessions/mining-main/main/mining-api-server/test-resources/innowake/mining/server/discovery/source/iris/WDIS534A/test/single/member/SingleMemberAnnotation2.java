/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534A.test.single.member;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.ElementType.TYPE_PARAMETER;
import static java.lang.annotation.ElementType.TYPE_USE;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.CLASS)
@Target({TYPE, ANNOTATION_TYPE, TYPE_PARAMETER, TYPE_USE})
public @interface SingleMemberAnnotation2 {

	String[] value();
}
