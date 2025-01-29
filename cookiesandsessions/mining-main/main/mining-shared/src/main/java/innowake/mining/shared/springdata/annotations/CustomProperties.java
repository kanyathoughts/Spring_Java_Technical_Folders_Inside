/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.springdata.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to identify a data field in the entity that should contain all the custom properties associated with a given entity class.
 * 
 * <pre>
 * @Entity
 * public class Demo {
 *		@CustomProperties
 *		protected Map<String, List<CustomProperty>> customProperties = new HashMap<>();
 *	
 *		## setter and getter method for customProperties field ##
 * }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface CustomProperties {

}
