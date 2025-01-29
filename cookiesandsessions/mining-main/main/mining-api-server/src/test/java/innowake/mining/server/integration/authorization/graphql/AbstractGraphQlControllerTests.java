/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.mockito.BDDMockito.given;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.graphql.controller.ControllerLocalContext;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.service.UserRoleService;
import innowake.mining.tags.AuthorizationTest;

/**
 * Base class for GraphQl Controller Test classes.
 */
@AuthorizationTest
public abstract class AbstractGraphQlControllerTests extends DatabaseRelatedTest {
	
	@Nullable
	@MockBean
	protected UserRoleService userRoleService;
	@Nullable
	protected HttpGraphQlTester tester;
	
	/**
	 * Get array of entities from Response
	 *
	 * @param <T> the Entity class
	 * @param response the {@linkplain Response}
	 * @param noOfElements the expected number of elements
	 * @param typeName the name of the GraphQl type
	 * @param clazz the Entity class
	 * @return array of parsed Entity objects
	 */
	static <T> T[] getEntries(final Response response, final int noOfElements, final String typeName, final Class<T> clazz) {
		response.path(typeName + ".totalElements").entity(Long.class).isEqualTo(Long.valueOf(noOfElements));
		@SuppressWarnings("unchecked")
		final T[] result = (T[]) Array.newInstance(clazz, noOfElements);
		for (int i = 0; i < noOfElements; i++) {
			final String path = String.format(typeName + ".content[%d]", Integer.valueOf(i));
			final T respModule = response.path(path).entity(clazz).get();
			result[i] = respModule;
		}
		return result;
	}
	
	/**
	 * Executes the given GraphQL query.
	 * 
	 * @param queryTemplate the query template to run
	 * @param projectId The project to query
	 * @param sortObject sort conditions in object format
	 * @param filterObject filtering conditions in object format
	 * @return {@link Response} from GraphQL with the query result
	 */
	Response executeQuery(final String queryTemplate, final EntityId projectId, @Nullable final List<Map<String, String>> sortObject, @Nullable final Object filterObject) {
		final Object filterValue = filterObject == null ? Collections.emptyMap() : filterObject;
		final Object sortValue = sortObject == null ? Collections.emptyList() : sortObject;
        return assertNotNull(tester)
				.document(queryTemplate)
				.variable("projectId", projectId.getNid())
				.variable("sortObject", sortValue)
				.variable("filterObject", filterValue)
				.execute();
	}

	/**
	 * Executes the given GraphQL query.
	 * 
	 * @param queryTemplate the query template to run
	 * @param projectId The project to query
	 * @param sortBy sort array of condition string
	 * @param sortObject sort conditions in object format
	 * @param filter filtering conditions
	 * @param filterObject filtering conditions in object format
	 * @return {@link Response} from GraphQL with the query result
	 */
	Response executeQueryWithOldFilter(final String queryTemplate, final EntityId projectId, @Nullable final List<String> sortBy,
			@Nullable final List<Map<String, String>> sortObject, @Nullable final String filter, @Nullable final Object filterObject) {
		if (filter != null) {
			throw new IllegalStateException("filter was decomissioned, please migrate it "+ filter);
		}
		if (sortBy != null) {
			throw new IllegalStateException("sortBy was decomissioned, please migrate it "+ sortBy);
		}			
		final String filterKey = "filterObject";
		final Object filterValue = filterObject == null ? Collections.emptyMap() : filterObject;
		final String sortKey = "sortObject";
		final Object sortValue = sortObject == null ? Collections.emptyList() : sortObject;
		final Response response = assertNotNull(tester)
				.document(queryTemplate)
				.variable("projectId", projectId.getNid())
				.variable(sortKey, sortValue)
				.variable(filterKey, filterValue)
				.execute();
		return response;
	}

	/**
	 * Sets the project accesses for the current user. The given {@code authProjectIds} are used for setting the user's {@link Authentication}. The given
	 * {@code userRoleProjectIds} are used for the {@link UserRoleService}. Both lists can differ.
	 * 
	 * <p>Usually the associated project IDs in the UserRoleService should match the project authorities. If a user has no authorization for a project
	 * ({@link SimpleGrantedAuthority}), the server responses with access {@code FORBIDDEN}. For explicitly testing of {@link ControllerLocalContext}
	 * and {@link UserRoleService} both project ID lists can differ.</p>
	 *
	 * @param authProjectIds project IDs for the {@link Authentication}
	 * @param userRoleProjectIds project IDs for the {@link UserRoleService}
	 */
	void setupProjectAccesses(final List<EntityId> authProjectIds, final List<EntityId> userRoleProjectIds) {
		final UserRoleService userRoleService = assertNotNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(EntityId.allNids(userRoleProjectIds));
		
		final List<SimpleGrantedAuthority> authorities = new ArrayList<>(authProjectIds.size() * 2);
		authProjectIds.forEach(projectId -> {
				authorities.add(new SimpleGrantedAuthority(String.format("client-1-project-%d-viewer", projectId.getNid())));
				authorities.add(new SimpleGrantedAuthority(String.format("client-1-project-%d-mining", projectId.getNid())));
		});
		
		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}

	/**
	 * Used to create maps for comparing expected with actual data,
	 * where expected data is retrived by index from an array
	 *
	 * Creates a Map of Key to Collection<Value> by retrieving the keys and values from the given functions for index 0..entryCount (exclusive)
	 * use Like:
	 *
	 * <pre>{@code Map<Long, Set<String>> actualMap = valuesToMap(4,
	 * 						idx -> response.path("annotations.content[" + idx + "].id").entity(Long.class).get(),
	 * 						idx -> getDataDictionaryEntries(response, idx));}</pre>
	 *
	 * @param entryCount the number of entries to generate
	 * @param keyMapper maps each 0..entryCount to the key, retrieved from the keyMapper
	 * @param valueMapper maps each 0..entryCount to the value, retrieved from the keyMapper
	 * @return a map of defined types
	 * @param <K> type of the Key
	 * @param <V> type of the Value
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static <K extends Comparable, V extends Collection> Map<K, V> valuesToMap(final int entryCount,
																  Function<Integer, ? extends K> keyMapper,
																  Function<Integer, ? extends V> valueMapper) {
		return Stream.iterate(0, x -> x < entryCount, x -> x + 1)
				.collect(Collectors.toMap(keyMapper,
						valueMapper,
						(m1, m2) -> {
							m1.addAll(m2);
							return m1;
						},
						() -> new TreeMap<>())
				);
	}
}
