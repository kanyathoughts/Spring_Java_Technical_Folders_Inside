/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation is used on a class to mark it as an entity mapped to a persistence instance of {@code OVertex} in DB.
 * 
 * <pre>
 * @Entity
 * public class Demo {
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface Entity {
	
	/**
	 * Name of the entity class is returned.
	 * 
	 * @return the name of the entity defined
	 */
	String name() default "";
	
	/**
	 * Denotes if the entity class is mapped to a vertex or edge.
	 * 
	 * @return true if the entity defined is an edge and not a vertex
	 */
	boolean isEdgeClass() default false;
	
	/**
	 * Denotes if the entity class is mapped to a document.
	 * 
	 * @return true if the entity defined is a document
	 */
	boolean isDocumentClass() default false;
}
