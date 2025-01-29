/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiPredicate;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.TypeDescriptor;
import org.springframework.core.convert.converter.GenericConverter;
import org.springframework.core.convert.converter.GenericConverter.ConvertiblePair;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.error.AmbiguousConverterException;
import innowake.mining.server.error.NoMatchingConverterException;

/**
 * Converter Service implementation class for source object to target object conversion or mapping.
 */
@Service
public class ConverterServiceImpl implements ConverterService {
	
	@Autowired(required = false)
	private List<GenericConverter> converters = Collections.emptyList();
	
	/* This is a cache for generic converters based on source and target types.*/
	private Map<String, GenericConverter> genericConverterCache = new ConcurrentHashMap<>();
	
	@Override
	public <S, T> T convert(final S source, final Class<T> targetType) {
		return lookupAndConvert(source, source.getClass(), targetType, false);
	}

	@Override
	public <S, T> T convert(final S source, final Class<? super S> sourceType, final Class<T> targetType) {
		return lookupAndConvert(source, sourceType, targetType, true);
	}
	
	/**
	 * Lookup method for fetching converters based on source type and target type. 
	 *
	 * @param <S> source class type
	 * @param <T> target class type
	 * @param sourceType source type
	 * @param targetType target type
	 * @param exactSourceTypeMatch {@code true} if exact match is required else {@code false}
	 * @return converter found for the given types
	 */
	protected <S, T> GenericConverter lookup(final Class<S> sourceType, final Class<T> targetType, final boolean exactSourceTypeMatch) {
		return chooseTheBestConverter(sourceType, targetType, exactSourceTypeMatch);
    }
	
	private <S, T> T lookupAndConvert(final S source, final Class<?> sourceType, final Class<T> targetType, final boolean exactSourceTypeMatch) {
		return targetType.cast(lookup(sourceType, targetType, exactSourceTypeMatch)
				.convert(source, TypeDescriptor.valueOf(sourceType), TypeDescriptor.valueOf(targetType)));
	}
	
	private <S, T> GenericConverter chooseTheBestConverter(final Class<S> sourceType, final Class<T> targetType, final boolean exactSourceTypeMatch) {
		GenericConverter validConverter = lookupConvertersInCache(sourceType, targetType, exactSourceTypeMatch);
		if (validConverter != null) {
			return validConverter;
		}
		if (exactSourceTypeMatch) {
			validConverter = findValidConverter(sourceType, targetType, (compatibleType, source) -> compatibleType.getSourceType().equals(source));
		} else {
			validConverter = findValidConverter(sourceType, targetType, (compatibleType, source) -> compatibleType.getSourceType().isAssignableFrom(source));
		}
		if(validConverter == null) {
			throw new NoMatchingConverterException(sourceType + " to " + targetType);
		}
		cacheLookedUpConverters(sourceType, targetType, exactSourceTypeMatch, validConverter);
		return validConverter;
	}
	
	@Nullable
	private GenericConverter findValidConverter(final Class<?> sourceType, final Class<?> targetType, 
			final BiPredicate<ConvertiblePair, Class<?>> filterCondition) {
		int converterCount = 0;
		GenericConverter validConverter = null;
		for(final GenericConverter genericConverter: converters) {
			final Set<ConvertiblePair> convertibleTypes = genericConverter.getConvertibleTypes();
			if (convertibleTypes != null) {
				for(ConvertiblePair convertibleType: convertibleTypes) {
					if (convertibleType.getTargetType().equals(targetType) && filterCondition.test(convertibleType, sourceType)) {
						validConverter = genericConverter;
						converterCount++;
					}
					if(converterCount > 1) {
						throw new AmbiguousConverterException(sourceType + " to " + targetType); 
					}
				}
			}
		}
		return validConverter;
	}
	
	/**
	 * This method will cache generic converter into the cache map, key string = sourceType + "To" + targetType + boolean(exactSourceTypeMatch).
	 * @param sourceType type of the source object
	 * @param targetType type of the target object
	 * @param exactSourceTypeMatch  boolean for exact match
	 * @param lookedUpConverter found valid converter
	 */
	private void cacheLookedUpConverters(final Class<?> sourceType, final Class<?> targetType, final boolean exactSourceTypeMatch, final GenericConverter lookedUpConverter) {
		genericConverterCache.put(getGenericConverterCacheKey(sourceType, targetType, exactSourceTypeMatch), lookedUpConverter);
	}

	private String getGenericConverterCacheKey(final Class<?> sourceType, final Class<?> targetType, final boolean exactSourceTypeMatch) {
		return sourceType + "To" + targetType + exactSourceTypeMatch;
	}
	
	@Nullable
	private GenericConverter lookupConvertersInCache(final Class<?> sourceType, final Class<?> targetType, final boolean exactSourceTypeMatch) {
		final String key = getGenericConverterCacheKey(sourceType, targetType, exactSourceTypeMatch);
		return genericConverterCache.get(key);
	}
}
