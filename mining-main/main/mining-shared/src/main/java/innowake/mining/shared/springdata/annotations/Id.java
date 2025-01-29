/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to identify a data field in the entity that should contain the auto
 * generated id by orient DB after the entity is persisted in DB.
 * 
 * <pre>
 * @Entity
 * public class Demo {
 *    @Id(sequence = "Demo_Sequence")
 *    private String demoId;
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Id {
	
	/**
	 * Sequence name in DB.
	 *
	 * @return sequence name
	 */
	String sequence() default "";
}
