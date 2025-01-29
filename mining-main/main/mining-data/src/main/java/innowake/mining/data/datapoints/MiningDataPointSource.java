/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.datapoints;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DataPointBuilder;

/**
 * Implement this interface on an {@link Component} class in order to register additional data points. The data points are made available
 * through the GraphQl query endpoint.
 */
public interface MiningDataPointSource {

	/**
	 * Construct a parameterized type that can be passed to {@link DataPointBuilder#type(Type)}.
	 *
	 * @param rawType the raw type (class)
	 * @param typeArguments the type arguments
	 * @return the {@code ParameterizedType}
	 */
	public static ParameterizedType parameterizedType(final Class<?> rawType, final Type... typeArguments) {
		return new ParameterizedType() {

			@Override
			public Type[] getActualTypeArguments() {
				return typeArguments;
			}

			@Override
			public Type getRawType() {
				return rawType;
			}

			@Override
			@Nullable
			public Type getOwnerType() {
				return null;
			}
			
		};
	}
	
	/**
	 * Define data points using the provided builder.
	 *
	 * @param builder the {@link MiningDataPointBuilder} to use
	 */
	public void provideDataPoints(final MiningDataPointBuilder builder);
}
