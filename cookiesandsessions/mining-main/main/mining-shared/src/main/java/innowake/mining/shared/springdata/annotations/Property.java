/**
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.springdata.annotations;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Annotation is used on an entity to map different names for the field than for the graph property.
 * 
 * <pre>
 * @Entity
 * public class Demo {
 * 		@Property("Demo_FieldName")
 * 	private String demoProperty;
 * }
 * </pre>
 */
@Retention(RUNTIME)
@Target(FIELD)
public @interface Property {

	String value();
}
