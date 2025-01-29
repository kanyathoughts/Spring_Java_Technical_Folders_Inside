/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.model;

import java.util.Optional;

/**
 * Interface to be implemented by all language specific model providers.
 * 
 * @param <T> the type of object being used to extract out model.
 */
public interface IModelProvider<T> {

	/**
	 * Interface for the model clients to access the model. Will return {@link Optional#empty()} if
	 * no module could be created.
	 *
	 * @param module
	 * @return ModelResult with status OK, TIMEOUT, ERROR (containing the model, if exists)
	 */
	Optional<ModelResult> getModel(T module);

	/**
	 * Called when editor has been changed
	 *
	 * @param module
	 */
	void updateModel(T module);

	/**
	 * Called when editor has been closed
	 *
	 * @param module
	 */
	void invalidateModel(T module);

}
