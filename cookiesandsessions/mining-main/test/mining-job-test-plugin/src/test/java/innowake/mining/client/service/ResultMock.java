/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.util.Optional;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;

/**
 * This "mocks" the {@link Result} of a REST call.
 * 
 * @param <T> the actual result type
 */
public class ResultMock<T> extends Result<T> {
	
	private final boolean isValid;
	@Nullable
	private final T value;

	/**
	 * Constructor.
	 * 
	 * @param valueType the {@link TypeReference} of the result type
	 * @param isValid {@code true} if the result should be treated as valid; {@code false} otherwise
	 * @param value the actual value to return
	 */
	public ResultMock(final TypeReference<T> valueType, final boolean isValid, @Nullable final T value) {
		super(valueType);
		this.isValid = isValid;
		this.value = value;
	}
	
	@Override
	public boolean isValid() {
		return isValid;
	}
	
	@Override
	public Optional<T> getValue() {
		return Optional.ofNullable(value);
	}

}
