/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

import java.util.stream.Stream;

/**
 * The pretty printer is used to improve the human readable format of a extracted module description. 
 */
@FunctionalInterface
public interface PrettyPrinter {

	/**
	 * Pretty print the given lines and return an overall module description string.
	 * 
	 * @param lines Access the the raw descriptive lines.
	 * @return The human readable module description.
	 */
	String print(Stream<String> lines);
}
