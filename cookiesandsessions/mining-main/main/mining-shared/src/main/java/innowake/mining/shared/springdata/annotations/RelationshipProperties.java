/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation is used on a class to mark it as an edge entity with properties mapped to a persistence instance of {@code OEdge} in DB.
 * 
 * <pre>
 * @RelationshipProperties
 * public class Demo {
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface RelationshipProperties {
	
}
