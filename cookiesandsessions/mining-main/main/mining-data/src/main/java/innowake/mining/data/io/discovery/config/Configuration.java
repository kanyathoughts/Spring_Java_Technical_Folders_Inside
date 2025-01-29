/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io.discovery.config;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Mark members to be part of the configuration.
 * Marked members will be written and loaded via configuration file.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({
 ElementType.FIELD,
 ElementType.METHOD
})
public @interface Configuration {

	/**
	 * The human readable name of this property in the configuration file
	 *
	 * @return The name.
	 */
	String name();
	
	/**
	 * The string representation of the default value.
	 * Must be supported by the from string method of the desired type. Example {@code Double#parseDouble(String)}
	 *
	 * @return The string default value.
	 */
	String defaultValue();
	
	/**
	 * A one liner comment to be written in the configuration file which helps the user to understand the property.
	 * A possible range definition would be helpful.
	 *
	 * @return A one liner comment describing this property.
	 */
	String comment();
	
	/**
	 * The title to be used in UI.
	 * 
	 * @return The title string.
	 */
	String title();
	
}
