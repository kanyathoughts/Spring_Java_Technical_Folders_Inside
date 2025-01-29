/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.server.ResponseStatusException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.PaginatedResponse;

/**
 * Utility class to aid in pagination of entities.
 */
public class PaginationUtil {

	private static final int DEFAULT_PAGE_SIZE = (Integer.MAX_VALUE - 1) / 2;
	private static final String PAGINATION_URL_PARAMS = "page=%d&size=%d";

	private PaginationUtil() {
		/* Empty constructor as this is a Utility class. */
	}

	/**
	 * Method to create the {@link Pageable} that can be used for an entity.
	 *
	 * @param page The page number
	 * @param size The page size
	 * @param sortBy The sorting conditions
	 * @return The {@link Pageable} instance
	 */
	public static Pageable constructPageable(final int page, int size, @Nullable final String[] sortBy) {
		final Sort allSorts = 
			sortBy == null 
			? null 
			: Sort.by(Arrays.stream(sortBy)
				.map(sort -> sort.split(";", 2))
				.map(array -> new Sort.Order(Sort.Direction.valueOf(array[1]),array[0]))
				.collect(Collectors.toList()));
		if(size == 0 && allSorts == null) {
			return Pageable.unpaged();
		} else {
			size = size == 0 ? DEFAULT_PAGE_SIZE : size;
			if (allSorts == null) {
				return PageRequest.of(page, size);
			} else {
				return PageRequest.of(page, size, allSorts);
			}
		}
	}

	public static Pageable constructPageable(@Nullable final Integer page, @Nullable final Integer size) {
		if(size == null || size.intValue() == 0) {
			return Pageable.unpaged();
		}
		int intPage = page == null ? 0 : page.intValue();
		return PageRequest.of(intPage, size.intValue());
	}
	
	public static Pageable constructPageable(final int page, final int size, final Sort sort) {
		return PageRequest.of(page, size == 0 ? DEFAULT_PAGE_SIZE : size, sort);
	}

	/**
	 * Method to return the pagination parameters for a URL with the specified page and size values.
	 *
	 * @param page The page number
	 * @param size The number of records in each page
	 * @return The pagination parameters that can be used in an URL
	 */
	public static String getPaginationParamsForUrl(final int page, final int size) {
		if (page < 0 || size < 0) {
			throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid value for pagination parameters");
		}
		return String.format(PAGINATION_URL_PARAMS, Integer.valueOf(page), Integer.valueOf(size));
	}

	/**
	 * Method to create a {@link Page Page} from the given {@link PaginatedResponse Paginated Response}.
	 * The Paginated Response is marked as {@link Nullable Nullable} as
	 * the method {@link ResponseEntity#getBody() ResponseEntity#getBody()} returns a Nullable value.
	 *
	 * @param paginatedResponse The {@link PaginatedResponse Paginated Response}
	 * @param page The page number
	 * @return A {@link Page Page} from the given {@link PaginatedResponse Paginated Response}
	 */
	public static <T> Page<T> getPageFromResponse(@Nullable final PaginatedResponse<T> paginatedResponse, final int page) {
		return paginatedResponse != null ? 
				new PageImpl<>(paginatedResponse.getContentList(), PaginationUtil.constructPageable(page, paginatedResponse.getContentList().size(), (String[]) null),
						paginatedResponse.getTotalSize()) :
				new PageImpl<>(Collections.emptyList());
	}
}
