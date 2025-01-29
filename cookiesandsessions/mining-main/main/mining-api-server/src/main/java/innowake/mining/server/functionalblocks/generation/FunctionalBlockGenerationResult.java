/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import java.util.Optional;

/**
 * Holds the result of a functional block generation.
 * @param <O> the type of additional data that is attached to the generated block
 * @see FunctionalBlockGeneration
 */
public class FunctionalBlockGenerationResult<O> {

	/**
	 * Defines the operation to perform with the generated functional block prototype.
	 */
	public enum Operation {
		/**
		 * A new block should be created based on the generated prototype.
		 */
		CREATE,
		/**
		 * An existing block should be updated based on the generated prototype.
		 */
		UPDATE,
		/**
		 * An existing block should be deleted based on the id in the prototype.
		 */
		DELETE
	}

	private final Operation operation;
	private final FunctionalBlockPojoPrototype functionalBlock;
	@Nullable
	private final O additionalData;

	/**
	 * Creates a new generation result with additional data.
	 *  @param operation the operation to perform on the generated block
	 * @param functionalBlock a prototype object of the generated block
	 * @param additionalData additional data that is attached to the generated block
	 */
	public FunctionalBlockGenerationResult(final Operation operation, final FunctionalBlockPojoPrototype functionalBlock, @Nullable final O additionalData) {
		this.operation = operation;
		this.functionalBlock = functionalBlock;
		this.additionalData = additionalData;
	}

	/**
	 * Creates a new generation result without additional data.
	 * @param operation  the operation to perform on the generated block
	 * @param functionalBlock a prototype object of the generated block
	 */
	public FunctionalBlockGenerationResult(final Operation operation, final FunctionalBlockPojoPrototype functionalBlock) {
		this.operation = operation;
		this.functionalBlock = functionalBlock;
		this.additionalData = null;
	}

	/**
	 * Returns the operation to perform on the generated block.
	 * @return the operation to perform on the generated block
	 */
	public Operation getOperation() {
		return operation;
	}

	/**
	 * Returns a prototype object of the generated block.
	 * @return a prototype object of the generated block
	 */
	public FunctionalBlockPojoPrototype getFunctionalBlock() {
		return functionalBlock;
	}

	/**
	 * Returns additional data that is attached to the generated block.
	 * @return additional data that is attached to the generated block
	 */
	public Optional<O> getAdditionalData() {
		return Optional.ofNullable(additionalData);
	}


}
