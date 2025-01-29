/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.graphql.execution.ErrorType;
import org.springframework.graphql.test.tester.GraphQlTester.Request;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.reactive.function.client.WebClientRequestException;

import graphql.schema.idl.errors.NotAnInputTypeError;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.authorization.AuthorizationTests;

/**
 * Base class for authorization tests for GraphQl endpoints (methods annotated with {@link QueryMapping}),
 * checking that either no errors or only expected errors (usually an error of type "Forbidden") are included in the response,
 * based on the user's roles and data being accessed.
 */
@TestInstance(Lifecycle.PER_CLASS)
public abstract class GraphQlAuthorizationTests extends AuthorizationTests {

	public static final String GRAPHQL_ENDPOINT = "/api/v2/graphql";

	@FunctionalInterface
	protected interface ResponseMatcher {

		void match(Response response);
	}

	@Autowired
	private WebApplicationContext webAppContext;
	
	@Nullable
	private HttpGraphQlTester tester;

	@BeforeAll
	public void init() {
		WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GRAPHQL_ENDPOINT)
				.build();

		tester = HttpGraphQlTester.create(client);
	}

	/**
	 * Parameterized test, executing the given GraphQL query with given parameters and roles and executing the given response matcher.
	 * 
	 * @param query the GraphQl query. the query may use variables in which case the values for these variables must be given via the {@code variables} parameter
	 * @param variables values for variables used in the GraphQl request
	 * @param roles the mining roles to use
	 * @param matcher a matcher that verifies the contents of the query
	 * 
	 */
	public void test(final String query, final Map<String, Object> variables, final List<GrantedAuthority> roles, final ResponseMatcher matcher) {
		setupSecurityContext(roles);
		
		final Request<?> request = assertNotNull(tester).document(query);
		for (final Map.Entry<String, Object> entry : variables.entrySet()) {
			request.variable(entry.getKey(), entry.getValue());
		}		
		try {
			matcher.match(request.execute());
		} catch (NotAnInputTypeError | WebClientRequestException e) {
//			-- The type 'MAP_STRING_java_lang_Object' [@313:1] is not an input type, but
//			This error occurs when theres a filter object defined, but no definition given.
//			you can either remove th efilter object for now to resolve the error, or define the Filter Map Schema in the respective DataPointSource, 
//			which implicitly defines the Usages and therefor the Maps schema.
			if (e.getMessage() != null && e.getMessage().contains("The type 'MAP_STRING_java_lang_Object'")) {
				throw new IllegalStateException("This error occurs when theres a filter object defined, but no definition given, Read Comment for explanation. ", e);
			} else {
				throw e;
			}
		}

	}
	
	/**
	 * Convenience method for constructing a key-value map containing variable assignments to be used with a GraphQl query.
	 * <p>
	 * The method must be provided with an alternating list of keys and values. Example:
	 * <pre>variables("clientId", 1, "projectId", 2)</pre>
	 * @param vars the list of variables, alternating between key and value
	 * @return the variable map
	 */
	protected static Map<String, Object> variables(Object... vars) {
		if (vars.length % 2 != 0) {
			throw new IllegalArgumentException("parameters() must be a list of key-value pairs");
		}
		final Map<String, Object> paramMap = new HashMap<>();
		for (int i = 0; i < vars.length; i += 2) {
			if (!(vars[i] instanceof String)) {
				throw new IllegalArgumentException("in arguments for parameters() the keys must be strings");
			}
			final String key = (String) vars[i];
			final Object value = vars[i + 1];
			paramMap.put(key, value);
		}
		return paramMap;
	}
	
	protected static ResponseMatcher expectSuccess() {
		return (response) -> {
			response.errors().verify();
		};
	}
	
	protected static ResponseMatcher expectError(final ErrorType errorExpected) {
		return (response) -> {
			response.errors().satisfy(errors -> {
				if (errors.isEmpty()) {
					throw new AssertionError("Response contained no errors while '" + errorExpected.name() + "' was expected.");
				}
				errors.forEach(error -> {
					if ( ! error.getErrorType().equals(errorExpected)) {
						throw new AssertionError("Response contains unexpected errors. Expected '" + errorExpected.name() + "'. Found '" + error.getErrorType() + "' in " + errors);
					}
				});
			});
		};
	}
	
	protected void setupSecurityContext(final List<GrantedAuthority> roles) {
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", roles);
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}
}
