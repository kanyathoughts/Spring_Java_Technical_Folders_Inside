/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;
import java.util.Objects;
import java.util.UUID;

import org.apache.http.Header;
import org.apache.http.HttpHeaders;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.message.BasicHeader;
import org.apache.http.protocol.HttpContext;
import org.apache.http.util.EntityUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.exceptions.StaleTokenException;
import innowake.mining.shared.access.EntityId;

/**
 * Base class for common HTTP REST service helping methods.
 * 
 * @param <T> the type of the service result 
 */
public abstract class RestService<T> {

	/**
	 * Logger {@link Logging#MINING_CLIENT_SERVICE}.
	 */
	protected static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);

	private final ConnectionInfo connectionInfo;
	private final HttpContext httpContext;

	@Nullable
	private String serviceUrl;

	private static final CloseableHttpClient httpClient = RestServiceHttpClient.HTTP_CLIENT;
	
	/**
	 * Creates a new instance with service endpoint URL.
	 * 
	 * @param connectionInfo the connection info to use
	 * @param serviceUrl the endpoint to use
	 */
	protected RestService(final ConnectionInfo connectionInfo, @Nullable final String serviceUrl) {
		this.connectionInfo = connectionInfo;
		this.serviceUrl = serviceUrl;
		httpContext = HttpClientContext.create();
		httpContext.setAttribute(HttpClientContext.COOKIE_STORE, connectionInfo.getCookieStore());
	}

	/**
	 * Creates a new instance without service endpoint URL.
	 * Require to call {@link #setServiceUrl(String)} before creating one of the request methods.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected RestService(final ConnectionInfo connectionInfo) {
		this(connectionInfo, null);
	}
	
	/**
	 * Returns the {@link ConnectionInfo}.
	 *
	 * @return the instance of {@link ConnectionInfo}
	 */
	public ConnectionInfo getConnectionInfo() {
		return connectionInfo;
	}
	
	/**
	 * Executes the service call.
	 *
	 * @return the service call result
	 * @throws IOException see {@link HttpClient#execute(HttpUriRequest)}
	 * @throws ClientProtocolException see {@link HttpClient#execute(HttpUriRequest)}
     * @throws ParseException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IllegalArgumentException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IOException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws java.nio.charset.UnsupportedCharsetException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
	 */
	public abstract Result<T> execute() throws IOException;
	
	/**
	 * Validates if service is proper set before executing.
	 * 
	 * @throws IllegalStateException if validation fails
	 */
	protected void validate() {
		/* default is empty */
	}
	
	/**
	 * Sets the service endpoint URL to use.
	 *
	 * @param serviceUrl the URL to use
	 */
	protected void setServiceUrl(final String serviceUrl) {
		this.serviceUrl = serviceUrl;
	}
	
	/**
	 * Creates a HTTP Post request and initializes it with the access token.
	 *
	 * @return the HTTP Post instance
	 */
	protected HttpPost httpPost() {
		final String apiUrl = buildUrl();
		LOG.debug(() -> "Calling POST " + apiUrl);
		final HttpPost httpPost = new HttpPost(apiUrl);
		httpPost.setHeader(getHeaderWithAuth());
		return httpPost;
	}
	
	/**
	 * Creates a HTTP Post request for MultiPart requests and initializes it with the access token.
	 *
	 * @return the HTTP Post instance
	 */
	protected HttpPost httpPostForMultiPart() {
	    final HttpPost httpPost = httpPost();
	    final String boundaryValue = generateBoundaryValue();
	    httpPost.setHeader(HttpHeaders.CONTENT_TYPE, "multipart/form-data; boundary=" + boundaryValue);
	    return httpPost;
	}

	private static String generateBoundaryValue() {
	    final String uuid = UUID.randomUUID().toString();
	    String boundaryValue = uuid.replace("-", "").toUpperCase();
	    boundaryValue = "----Boundary-" + boundaryValue;
	    return boundaryValue;
	}

	/**
	 * Creates a HTTP Get request and initializes it with the access token.  
	 *
	 * @return the HTTP Get instance
	 */
	protected HttpGet httpGet() {
		final String apiUrl = buildUrl();
		LOG.debug(() -> "Calling GET " + apiUrl);
		final HttpGet httpGet = new HttpGet(apiUrl);
		httpGet.setHeader(getHeaderWithAuth());
		return httpGet;
	}

	/**
	 * Creates a HTTP Put request and initializes it with the access token.  
	 *
	 * @return the HTTP Put instance
	 */
	protected HttpPut httpPut() {
		final String apiUrl = buildUrl();
		LOG.debug(() -> "Calling PUT " + apiUrl);
		final HttpPut httpPut = new HttpPut(apiUrl);
		httpPut.setHeader(getHeaderWithAuth());
		return httpPut;
	}

	/**
	 * Creates a HTTP Delete request and initializes it with the access token.  
	 *
	 * @return the HTTP Delete instance
	 */
	protected HttpDelete httpDelete() {
		final String apiUrl = buildUrl();
		LOG.debug(() -> "Calling DELETE " + apiUrl);
		final HttpDelete httpDelete = new HttpDelete(apiUrl);
		httpDelete.setHeader(getHeaderWithAuth());
		return httpDelete;
	}

	/**
	 * Executes the request and creates a matching response.
	 *
	 * @param request the request to execute
	 * @return the result with status code, status line and if valid the instance of the DTO
	 * @throws IOException see {@link HttpClient#execute(HttpUriRequest)}
	 * @throws ClientProtocolException see {@link HttpClient#execute(HttpUriRequest)}
     * @throws ParseException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IllegalArgumentException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IOException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws java.nio.charset.UnsupportedCharsetException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
	 */
	protected Result<T> execute(final HttpUriRequest request) throws IOException {
		return execute(request, new TypeReference<T>() {});
	}
	
	/**
	 * Executes the request and creates a matching response.
	 *
	 * @param request the request to execute
	 * @param type the expected return type DTO. The result does contain the instance of this type if the request was successful. 
	 * @return the result with status code, status line and if valid the instance of the DTO
	 * @throws IOException see {@link HttpClient#execute(HttpUriRequest)}
	 * @throws ClientProtocolException see {@link HttpClient#execute(HttpUriRequest)}
     * @throws ParseException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IllegalArgumentException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IOException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws java.nio.charset.UnsupportedCharsetException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
	 */
	protected Result<T> execute(final HttpUriRequest request, final TypeReference<T> type) throws IOException {
		return execute(request, type, null);
	}
	
	/**
	 * Executes the request and creates a matching response.
	 *
	 * @param request the request to execute
	 * @param type the expected return type DTO. The result does contain the instance of this type if the request was successful. 
	 * @param customResponseHandler the {@link CustomResponseHandler} to handle responses having a different content type than JSON
	 * @return the result with status code, status line and if valid the instance of the DTO
	 * @throws IOException see {@link HttpClient#execute(HttpUriRequest)}
	 * @throws ClientProtocolException see {@link HttpClient#execute(HttpUriRequest)}
     * @throws ParseException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IllegalArgumentException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws IOException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
     * @throws java.nio.charset.UnsupportedCharsetException see {@link EntityUtils#toString(org.apache.http.HttpEntity)}
	 */
	protected Result<T> execute(final HttpUriRequest request, final TypeReference<T> type, @Nullable final CustomResponseHandler<T> customResponseHandler) throws IOException {
		final Result<T> result = new Result<>(type);
		try (@Nullable final CloseableHttpResponse response = httpClient.execute(request, httpContext)) {
			result.init(response, customResponseHandler);
		}
		return result;
	}
	
	protected String buildUrl() {
		if (serviceUrl == null) {
			throw new IllegalStateException("The service endpoint URL is not set.");
		}
		return connectionInfo.getUrl() + serviceUrl;
	}
	
	/**
	 * Constructs a new {@link BasicHeader} with {@link HttpHeaders#AUTHORIZATION} header set.
	 *
	 * @return the {@link BasicHeader} with authorization key set
	 * @throws StaleTokenException if bearer token could not be retrieved due to user's session being expired or revoked 
	 */
	@Nullable
	protected Header getHeaderWithAuth() {
		return new BasicHeader(HttpHeaders.AUTHORIZATION, "Bearer " + connectionInfo.getToken());
	}

	/**
	 * Returns the the parameter string depending on the type of ID in the given {@code entityId} for the service call.
	 *
	 * @param entityId the {@link EntityId} to encode
	 * @return string parameter for service call
	 * @throws IllegalStateException if {@code entityId} is {@code null}
	 */
	protected String encode(@Nullable final EntityId entityId) {
		if (entityId == null) {
			throw new IllegalStateException("Entity id must be set.");
		}

		return Objects.requireNonNull(entityId.value(), "EntityId must either contain a unique or numeric id").toString();
	}
}
