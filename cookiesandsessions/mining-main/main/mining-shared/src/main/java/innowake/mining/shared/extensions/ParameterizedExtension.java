/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.extensions;

import java.util.List;

import innowake.mining.shared.io.ParameterDescription;

/**
 * This interface can be implemented to describe extension parameters.
 */
public interface ParameterizedExtension {
	
	/**
	 * Get {@link List} of {@link ParameterDescription} for {@link ParameterizedExtension}.
	 * 
	 * @return {@link List} of {@link ParameterDescription}
	 */
	List<ParameterDescription> getParameterDescriptions();
}