/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a field to be transient for the mapping framework. 
 * Thus the property will not be persisted and not further inspected by the mapping framework.
 * 
 * <pre>
 * @Entity
 * public class Demo {
 *    @Transient
 *    private String demoId;
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(value = { FIELD, METHOD, ANNOTATION_TYPE })
public @interface Transient {
}
