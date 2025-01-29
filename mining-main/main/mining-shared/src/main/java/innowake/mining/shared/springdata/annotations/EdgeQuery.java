/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import innowake.mining.shared.springdata.EdgeDirection;

/**
 * Annotation to denote if the entities are linked by an edge and appropriate named query should be constructed.
 * 
 * <pre>
 * public class ProjectRepository extends OrientRepository<Project> {
 * 
 * 	@EdgeQuery
 *.	List<Project> findProjectByClientClientName(final String clientName);
 * }
 * </pre>
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface EdgeQuery {

	/**
	 * Denotes the direction of the edge type.
	 *
	 * @return the direction of edge
	 */
	EdgeDirection direction() default EdgeDirection.OUT;

}
