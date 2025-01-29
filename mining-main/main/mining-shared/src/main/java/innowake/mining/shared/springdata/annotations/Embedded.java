/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation is used on a class to mark it as an entity of embedded type, which does not have a record id.
 * 
 * <pre>
 * @Embedded
 * public class Demo {
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface Embedded {
	
	/**
	 * Name of the entity class is returned.
	 * 
	 * @return the name of the entity defined
	 */
	String name() default "";
	
}
