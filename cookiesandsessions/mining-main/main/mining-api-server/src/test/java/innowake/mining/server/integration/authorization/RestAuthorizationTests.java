/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.params.provider.Arguments.arguments;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.request;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.entity.ContentType;
import org.ff4j.FF4j;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.provider.Arguments;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMultipartHttpServletRequestBuilder;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.server.service.UserNameService;
import innowake.mining.shared.model.FeatureId;
import org.springframework.web.multipart.MultipartFile;

/**
 * Base class for authorization tests for REST endpoints, checking the correct status code based on the roles and data being accessed.
 * <p>
 * Sub-classes must implement the following methods for providing the parameters:
 * <ul>
 * <li>{@code static Stream<Arguments> testCases}
 * <li>{@code static Stream<Arguments> multiPartTestCases}
 * </ul>
 * <p>
 * which must return the stream of arguments.
 * <p>
 * Example for {@code static Stream<Arguments> testCases}
 * <p>
 * {@code
 * arguments(GET, url(ANNOTATION_CATEGORY_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK)
 * }
 * <p>
 * <p>
 * Example for {@code static Stream<Arguments> multiPartTestCases}
 * <p>
 * {@code
 * arguments(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(admin()), CREATED, "file", "foo.jpg", ContentType.MULTIPART_FORM_DATA)
 * }
 * <p>
 * Use {@link RestAuthorizationTests#emptyTestCases()} and/or {@link RestAuthorizationTests#emptyMultiPartTestCases()} in case where there are no tests to be run.
 */
public abstract class RestAuthorizationTests extends AuthorizationTests {

	@Autowired
	protected MockMvc mockMvc;
	
	@Nullable
	@MockBean
	private UserNameService userNameService;
	
	@Autowired
	private FF4j ff4j;
	
	private Map<String, Boolean> featureMap = Collections.emptyMap();
	
	/**
	 * Parameterized test, calling the given URL with the defined parameters and roles and asserting the given status code.
	 * 
	 * @param method the HTTP method to use
	 * @param url the URL to call
	 * @param uriVars the URL variables necessary
	 * @param params the request parameters
	 * @param content the dummy content to send with the request
	 * @param roles the mining roles to use
	 * @param status the HTTP status code expected
	 * 
	 * @throws Exception if the mocked call fails
	 */
	public void test(@Nullable final HttpMethod method, final String url, final List<Object> uriVars, final MultiValueMap<String, String> params,
			final Content content, final List<GrantedAuthority> roles, final HttpStatus status) throws Exception {
		/* Short circuit when called with emptyTestCases */
		if (method == null) return;
		
		Mockito.when(assertNotNull(userNameService).lookupUserName(ArgumentMatchers.anyString()))
		.thenAnswer(i -> new UserNameService.LookupResult(i.getArgument(0), UserNameService.LookupResult.Status.OK));
		
		mockMvc.perform(
				request(method, url, uriVars.toArray())
				.with(miningUser(roles))
				.contentType(MediaType.APPLICATION_JSON)
				.content(content.getValue())
				.params(params)
				)
		.andDo(print())
		.andExpect(status().is(status.value()));
	}
	
	/**
	 * Parameterized tests for Multipart requests.
	 *
	 * @param method the HTTP method to use
	 * @param url the URL to call
	 * @param uriVars the URL variables necessary
	 * @param params the request parameters
	 * @param roles the mining roles to use
	 * @param status the HTTP status code expected
	 * @param paramName the parameter name in the endpoint method
	 * @param originalFileName the original file name of the multipart file
	 * @param contentType the content type of the file
	 * 
	 * @throws Exception if the mocked call fails
	 */
	public void multiPartTests(@Nullable final HttpMethod method, final String url, final List<Object> uriVars, final MultiValueMap<String, String> params,
			final List<GrantedAuthority> roles, final HttpStatus status, final String paramName, final String originalFileName, final ContentType contentType) throws Exception {
		final MockMultipartFile file = new MockMultipartFile(paramName, originalFileName, contentType.getMimeType(), "some bytes".getBytes());
		multiPartTests(method, url, uriVars, params, roles, status, file);
	}

	/**
	 * Parameterized tests for Multipart requests with multiple multipart values.
	 *
	 * @param method the HTTP method to use
	 * @param url the URL to call
	 * @param uriVars the URL variables necessary
	 * @param params the request parameters
	 * @param roles the mining roles to use
	 * @param status the HTTP status code expected
	 * @param files the multipart files to send
	 *
	 * @throws Exception if the mocked call fails
	 */
	public void multiPartTests(@Nullable final HttpMethod method, final String url, final List<Object> uriVars, final MultiValueMap<String, String> params,
			final List<GrantedAuthority> roles, final HttpStatus status, final MockMultipartFile... files) throws Exception {
		/* Short circuit when called with emptyMultiPartTestCases */
		if (method == null) return;

		Mockito.when(assertNotNull(userNameService).lookupUserName(ArgumentMatchers.anyString()))
				.thenAnswer(i -> new UserNameService.LookupResult(i.getArgument(0), UserNameService.LookupResult.Status.OK));

		final MockMultipartHttpServletRequestBuilder requestBuilder = multipart(url, uriVars.toArray());

		for (final MockMultipartFile file : files) {
			requestBuilder.file(file);
		}

		mockMvc.perform(requestBuilder
						.with(request -> {
							request.setMethod(method.name());
							return request;
						})
						.contentType(MediaType.MULTIPART_FORM_DATA_VALUE)
						.params(params)
						.with(miningUser(roles))
				)
				.andDo(print())
				.andExpect(status().is(status.value()));
	}
	
	@BeforeEach
	void init() {
		featureMap = new HashMap<>();
		/* Store all feature settings and activate all */
		Stream.of(FeatureId.values()).forEach(feature -> {
			final String featureId = feature.getId();
			featureMap.put(featureId, Boolean.valueOf(ff4j.getFeature(featureId).isEnable()));
			ff4j.enable(featureId);
		});
	}
	
	@AfterEach
	void tearDown() {
		/* Restore all features */
		featureMap.entrySet().forEach(entry -> {
			if (entry.getValue().booleanValue()) {
				ff4j.enable(entry.getKey());
			} else {
				ff4j.disable(entry.getKey());
			}
		});
	}
	
	/**
	 * Convenience method for preparing the given URL for usage in the test call.
	 *
	 * @param url the URL to use for the test
	 * @return the prepared URL 
	 */
	protected static String url(final String url) {
		return StringUtils.prependIfMissing(url, "/api");
	}
	
	/**
	 * Convenience method for defining the URI variables of the specified URL.
	 *
	 * @param params the variables to define
	 * @return a list of URI variables
	 */
	protected static List<Object> uriVars(final Object... params) {
		return Arrays.asList(params);
	}
	
	/**
	 * Convenience method for usage when no URI variables are required.
	 *
	 * @return an empty list of URI variables
	 */
	protected static List<Object> noUriVars() {
		return Collections.emptyList();
	}
	
	/**
	 * Convenience method for defining the request parameters.
	 *
	 * @param params the parameters as tuples, where the first element is the parameter name and the second is the parameter value
	 * @return a multi value map for usage in the mocked call
	 */
	protected static MultiValueMap<String, String> params(final List<Tuple2<String, String>> params) {
		final MultiValueMap<String, String> result = new LinkedMultiValueMap<>();
		for (final Tuple2<String, String> param : params) {
			result.add(param.a, param.b);
		}
		return result;
	}
	
	/**
	 * Convenience method for usage when no query parameters are required.
	 *
	 * @return an empty multi value map of query parameters
	 */
	protected static MultiValueMap<String, String> noParams() {
		return new LinkedMultiValueMap<>();
	}
	
	/**
	 * @return an empty stream of test cases for usage in {@code static Stream<Arguments> testCases}
	 */
	protected static Stream<Arguments> emptyTestCases() {
		return Stream.of(arguments(null, null, null, null, null, null, null));
	}
	
	/**
	 * @return an empty stream of test cases for usage in {@code static Stream<Arguments> multiPartTestCases}
	 */
	protected static Stream<Arguments> emptyMultiPartTestCases() {
		return Stream.of(arguments(null, null, null, null, null, null, null, null, null));
	}
	
	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

	/**
	 * Defines how the request body is sent.
	 */
	enum Content {
		/**
		 * Send an empty JSON object.
		 */
		OBJECT("{}"),
		/**
		 * Send an empty JSON list.
		 */
		LIST("[]")
		;

		private String value;

		Content(final String value) {
			this.value = value;
		}

		public String getValue() {
			return value;
		}
	}
}
