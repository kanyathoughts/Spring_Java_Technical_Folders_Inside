/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import innowake.mining.shared.springdata.EdgeDirection;

/**
 * It defines the relationship between two entity classes.
 * Establish a link between two vertices in database, it is the name of the edge class.
 * 
 * <pre>
 * @Entity
 * public class Employee {
 *    @Relationship(name = "salaryAccount", direction = EdgeDirection.OUT)
 *    private Account account;
 *    
 *    @Relationship(name = "residence", direction = EdgeDirection.IN)
 *    private List<Address> address;
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface Relationship {
	
	/**
	 * Maps to the name of an edge class in DB.
	 * 
	 * @return the name of the edge defined
	 */
	String name() default ""; 
	
	/**
	 * Denotes the direction of the edge type.
	 *
	 * @return the direction of edge
	 */
	EdgeDirection direction() default EdgeDirection.OUT;
	
	/**
	 * Sequence name in DB.
	 *
	 * @return sequence name
	 */
	String sequence() default "";
}
