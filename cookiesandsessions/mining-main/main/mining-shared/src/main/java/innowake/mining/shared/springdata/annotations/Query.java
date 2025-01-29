/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define query string that needs to be executed.
 * 
 * <pre>
 * public class PersonRepository extends OrientRepository<Person> {
 *    	@Query(value = "select count(*) from person where firstName = ? and active = ?", count = true)
 *  	Long countByFirstNameAndActive(String firstName, Boolean active);
 *  
 *  	@Query("select * from person where emailId.toLowerCase() = ?.toLowerCase()")
 *		Person findByEmailIdIgnoreCase(final String emailId);
 *
 *		@Query(value = "select * from person where age in ?")
 *		List<Person> findPersonByAgeIn(final Collection<Integer> age);
 * }
 * </pre>
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Query {

	/**
	 * Defines the Orient query to be executed when the annotated method is called.
	 *
	 * @return the query to be executed
	 */
	String value() default "";

	/**
	 * Defines a special count query that shall be used for pagination queries to lookup the total number of elements for
	 * a page. If none is configured the count query will be derived from the method name.
	 *
	 * @return true if it executes a count query
	 */
	boolean count() default false;

	/**
	 * Defines a special count query that shall be used for pagination queries to lookup the total number of elements for
	 * a page. If none is configured we will derive the count query from the original query.
	 */
	String countQuery() default "";

	/**
	 * Defines a special count query using the alias that shall be used for pagination queries to lookup the total number of elements for
	 * a page. If none is configured we will derive the count query from the original query.
	 */
	String countQueryAlias() default "count(*)";
}