/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import innowake.mining.server.error.AmbiguousConverterException;
import innowake.mining.server.error.NoMatchingConverterException;

/**
 * Converter Service interface for mapping from source object to target object.
 */
public interface ConverterService {
	/**
	 * Converts given {@code source} object into an object of the target type.
	 * <p>
	 * This method throws {@link NoMatchingConverterException} if no known converter can provide the conversion
	 * from the given source to the given target type.
	 * <p>
	 * This method throws {@link AmbiguousConverterException} if more than one converter can provide the conversion from source to target.
	 * In this case, use the overload {@link #convert(Object, Class, Class)} to disambiguate the source type.
	 *
	 * @param <T> type of the target object
	 * @param <S> type of the source object
	 * @param source the source object
	 * @param targetType the desired target type
	 * @return an object of the desired target type, with properties set from the source object
	 */
	<S, T> T convert(S source, Class<T> targetType);

	/**
	 * Converts given {@code source} object into an object of the target type. During the conversion, the source value will be treated
	 * (only) as an instance of the given {@code sourceType}.
	 * <p>
	 * This method throws {@link NoMatchingConverterException} if no known converter can provide the conversion
	 * from the given source to the given target type.
	 *
	 * @param <T> type of the target object
	 * @param <S> type of the source object
	 * @param source the source object
	 * @param sourceType the source object will be narrowed to this type when selecting a converter
	 * @param targetType the desired target type
	 * @return an object of the desired target type, with properties set from the source object
	 */
	<S, T> T convert(S source, Class<? super S> sourceType, Class<T> targetType);
	
}
