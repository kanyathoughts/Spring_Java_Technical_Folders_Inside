/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.Optional;

import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpStatus;
import org.apache.http.ParseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.impl.EnglishReasonPhraseCatalog;
import org.apache.http.util.EntityUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.Logging;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.CustomErrorResponse;

/**
 * Holds the return values of the REST call.
 * 
 * @param <T> the DTO type of the response body. Could be accessed via {@link #getValue()}.
 */
public class Result<T> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	
	private int statusCode;
	@Nullable
	private String statusReasonPhrase;
	@Nullable
	private T value;
	@Nullable 
	private String body;
	private final TypeReference<T> valueType;
	private boolean valid = false;
	@Nullable
	private CustomErrorResponse customErrorResponse;
	
	@Nullable
	public CustomErrorResponse getCustomErrorResponse() {
		return customErrorResponse;
	}

	protected Result(final TypeReference<T> valueType) {
		this.valueType = valueType;
	}
	
	/**
	 * Initializes this result object with an {@link CloseableHttpResponse}.
	 *
	 * @param response the response for initializing
     * @throws ParseException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IllegalArgumentException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IOException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws java.nio.charset.UnsupportedCharsetException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
	 */
	void init(@Nullable final CloseableHttpResponse response) throws IOException {
		init(response, null);
	}
	
	/**
	 * Initializes this result object with an {@link CloseableHttpResponse}.
	 *
	 * @param response the response for initializing
	 * @param customResponseHandler the {@link CustomResponseHandler} to handle responses having a different content type than JSON
     * @throws ParseException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IllegalArgumentException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IOException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws java.nio.charset.UnsupportedCharsetException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
	 */
	void init(@Nullable final CloseableHttpResponse response, @Nullable final CustomResponseHandler<T> customResponseHandler) throws IOException {
		if (response == null) {
			LOG.error(() -> "Method failed: response is null");
		} else {
			statusCode = response.getStatusLine().getStatusCode();
			statusReasonPhrase = EnglishReasonPhraseCatalog.INSTANCE.getReason(statusCode, null);
			LOG.debug(() -> "Request HTTP status code is " + statusCode);
			final HttpEntity entity = response.getEntity();
			final Header contentType = entity != null ? entity.getContentType() : null;
			/* JSON is the default, so we always assume true for other cases */
			final boolean isJsonResponse = (contentType == null || contentType.getValue().contains("json"));
			
			if (entity != null) {
				body = isJsonResponse ? EntityUtils.toString(entity) : "customResponse";
			} else {
				body = null;
			}
			if (statusCode == HttpStatus.SC_OK || statusCode == HttpStatus.SC_CREATED || statusCode == HttpStatus.SC_NO_CONTENT
					|| statusCode == HttpStatus.SC_ACCEPTED) {
				LOG.debug(() -> "Request HTTP status body is " + body);
				valid = true;
				if (isJsonResponse) {
					if (StringUtils.isNotBlank(body)) {
						final var mapper = PojoMapper.jsonReader();
						final JsonNode jsonNode = mapper.readTree(body);
						/* Map old || new page(d) to content */
						if (jsonNode.hasNonNull("pageable") || jsonNode.has("content") &&
																jsonNode.hasNonNull("limit") &&
																jsonNode.hasNonNull("offset") &&
																jsonNode.hasNonNull("size") ) {
							value = mapper.readValue(jsonNode.get("content").traverse(), valueType);
						} else {
							value = PojoMapper.jsonReaderFor(valueType).readValue(body);
						}
					} else {
						value = null;
					}
				} else {
					if (customResponseHandler == null) {
						throw new IllegalStateException("Cannot handle response of type " + assertNotNull(contentType).getValue() + " without providing a "
								+ CustomResponseHandler.class.getCanonicalName());
					}
					value = customResponseHandler.handleResponse(response);
				}
			} else {
				if (isJsonResponse) {
					customErrorResponse = StringUtils.isNotBlank(body)
							? PojoMapper.jsonReaderFor(CustomErrorResponse.class).readValue(body) : customErrorResponse;
					if (customErrorResponse != null) {
						body = customErrorResponse.getMessage();
					}
				}
				valid = false;
			}
		}
	}
	
	/**
	 * Is valid if the response type is of {@link HttpStatus#SC_OK} or {@link HttpStatus#SC_CREATED}.
	 * The result value is only created if the response is valid.
	 * 
	 * @return true or false
	 */
	public boolean isValid() {
		return valid;
	}
	
	/**
	 * Gets the HTTP status code from the call.
	 * Could be compared against {@link HttpStatus}.
	 *
	 * @return the status code
	 */
	public int getStatusCode() {
		return statusCode;
	}
	
	/**
	 * Gets the body of the HTTP response.
	 *
	 * @return the body string
	 */
	public Optional<String> getStatusBody() {
		return Optional.ofNullable(StringEscapeUtils.unescapeJava(body));
	}
	
	/**
	 * Gets the REST status as message with format {@code Server Response Code (" + statusCode + ") " + body}.
	 *
	 * @return the status message
	 */
	public String getExtendedStatusMessage() {
		return "Server response code (" + statusCode + ") " + getStatusBody().orElse(null);
	}
	
	/**
	 * Gets the REST status message with format {@code Server Response: " + statusCode + " " + statusReasonPhrase}. 
	 *
	 * @return the status message
	 */
	public String getStatusMessage() {
		final StringBuilder result = new StringBuilder("Server response: ");
		result.append(statusCode);
		result.append(" ");
		result.append(statusReasonPhrase);
		return result.toString();
	}
	
	/**
	 * Gets the deserialized value object of the endpoint call.
	 * Is null if the response code is not {@link HttpStatus#SC_OK} or {@link HttpStatus#SC_CREATED}.
	 *
	 * @return the value
	 */
	public Optional<T> getValue() {
		return Optional.ofNullable(value);
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE);
		builder.append("statusCode", statusCode);
		builder.append("statusBody", body);
		builder.append("value", value);
		builder.append("valueType", valueType.getType());
		builder.append("valid", valid);
		return builder.toString();
	}
}
