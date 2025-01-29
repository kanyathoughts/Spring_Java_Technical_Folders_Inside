/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.converter;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.TypeDescriptor;
import org.springframework.core.convert.converter.GenericConverter;
import org.springframework.stereotype.Component;

import java.util.Set;

/**
 * Converter that serializes {@link SearchOrder} to JSON.
 */
@Component
public class SearchOrderToJsonConverter implements GenericConverter {

	private final ObjectMapper objectMapper;

	@Autowired
	public SearchOrderToJsonConverter(final ObjectMapper objectMapper) {
		this.objectMapper = objectMapper;
	}

	@Nullable
	@Override
	public Set<ConvertiblePair> getConvertibleTypes() {
		return Sets.newHashSet(new ConvertiblePair(String.class, SearchOrder.class), new ConvertiblePair(SearchOrder.class, String.class));
	}

	@Nullable
	@Override
	public Object convert(@Nullable final Object source, final TypeDescriptor sourceType, final TypeDescriptor targetType) {
		try {
			if (sourceType.getType().isAssignableFrom(SearchOrder.class)) {
				return objectMapper.writeValueAsString(source);
			} else if (sourceType.getType().isAssignableFrom(String.class)) {
				return objectMapper.readValue((String) source, SearchOrder.class);
			} else {
				throw new IllegalArgumentException("SearchOrderToJsonConverter cannot convert from " + sourceType);
			}
		} catch (JsonProcessingException e) {
			throw new IllegalArgumentException("Unable to convert SearchOrder from/to JSON ", e);
		}
	}
}
