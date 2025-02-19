/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to identify a {@link String} field in the entity that should contain the 
 * {@code ORID}'s record id generated by orient DB after the entity is persisted in DB.
 * 
 * <pre>
 * @Entity
 * public class Demo {
 * 	  @RId
 *    private String rId;
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface RId {
}
