/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.error;

import java.time.LocalDateTime;

import javax.servlet.http.HttpServletRequest;

import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.orientechnologies.orient.core.exception.OValidationException;

import cz.jirutka.rsql.parser.RSQLParserException;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.server.Logging;
import innowake.mining.shared.Definable;
import innowake.mining.shared.model.CustomErrorResponse;

/**
 * Exception handler advice class for all controllers.
 */
@ControllerAdvice
public class CustomExceptionHandler extends ResponseEntityExceptionHandler {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);
	
	/**
	 * Generic error handler for exceptions thrown from controller methods, to prevent leak of environment information. 
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return entity with {@link HttpStatus#INTERNAL_SERVER_ERROR}
	 */
	@ExceptionHandler(Exception.class)
	public ResponseEntity<CustomErrorResponse> handleException(final HttpServletRequest request, final Exception exception) {
		LOG.error(() -> "Unhandled exception thrown from controller method while accessing " + request.getRequestURI(), exception);
		return createResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, request, exception);
	}
	
	/**
	 * Maps an {@link OValidationException} to {@link HttpStatus#BAD_REQUEST}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#BAD_REQUEST}
	 */
	@ExceptionHandler(OValidationException.class)
	public ResponseEntity<CustomErrorResponse> handleOValidationException(final HttpServletRequest request, final OValidationException exception) {
		LOG.error(() -> "OValidationException thrown from controller method while accessing " + request.getRequestURI(), exception);
		return createResponseEntity(HttpStatus.BAD_REQUEST, request, exception);
	}
	
	/**
	 * Maps an {@link innowake.mining.shared.Definable.ValueException} to {@link HttpStatus#BAD_REQUEST}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#BAD_REQUEST}
	 */
	@ExceptionHandler(Definable.ValueException.class)
	public ResponseEntity<CustomErrorResponse> handleDefinableValueException(final HttpServletRequest request, final Definable.ValueException exception) {
		LOG.error(() -> "Definable.ValueException thrown from controller method while accessing " + request.getRequestURI(), exception);
		return createResponseEntity(HttpStatus.BAD_REQUEST, request, exception);
	}
	
	/**
	 * Maps an {@link DataAccessException} to {@link HttpStatus#BAD_REQUEST}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#BAD_REQUEST}
	 */
	@ExceptionHandler(DataAccessException.class)
	public ResponseEntity<CustomErrorResponse> handleDataAccessException(final HttpServletRequest request, final DataAccessException exception) {
		LOG.error(() -> "DataAccessException thrown from controller method while accessing " + request.getRequestURI(), exception);
		return createResponseEntity(HttpStatus.BAD_REQUEST, request, exception);
	}
	
	/**
	 * Error handler for {@link ResponseStatusException}. Required to restore Spring's default behavior, as otherwise
	 * the default error handler is triggered and the status code is mapped to 500 instead of the requested status code.
	 *
	 * @param exception the {@code ResponseStatusException}
	 * @return response entity with appropriate status code and reason phrase
	 */
	@ExceptionHandler(ResponseStatusException.class)
	public ResponseEntity<CustomErrorResponse> handleResponseStatusException(final ResponseStatusException exception) {
		return createResponseEntity(exception.getStatus(), exception);
	}

	/**
	 * Error handler for {@link UserFacingException}. Used to explicitly pass original error messages to the UI.
	 *
	 * @param exception the {@code UserFacingException}
	 * @return response entity with appropriate status code and reason phrase
	 */
	@ExceptionHandler(UserFacingException.class)
	public ResponseEntity<CustomErrorResponse> handleUserFacingException(final UserFacingException exception) {
		return createResponseEntity(exception);
	}
	
	/**
	 * Maps an {@link javax.persistence.EntityNotFoundException} to {@link HttpStatus#NOT_FOUND}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#NOT_FOUND}
	 */
	@ExceptionHandler({javax.persistence.EntityNotFoundException.class})
	public ResponseEntity<CustomErrorResponse> handleEntityNotFoundException(final HttpServletRequest request,
			final javax.persistence.EntityNotFoundException exception) {
		return createResponseEntity(HttpStatus.NOT_FOUND, request, exception);
	}
	
	/**
	 * Maps an {@link HttpClientErrorException} to an exception with the appropriate Status Code.
	 *
	 * @param request The request while the exception occurred
	 * @param exception The exception that occurred
	 * @return Entity with appropriate Status Code extracted from the exception that occurred
	 */
	@ExceptionHandler({HttpClientErrorException.class})
	public ResponseEntity<CustomErrorResponse> handleHttpClientErrors(final HttpServletRequest request, final HttpClientErrorException exception) {
		return createResponseEntity(exception.getStatusCode(), request, exception);
	}

	/**
	 * Maps an {@link ConstraintViolationException} to {@link HttpStatus#BAD_REQUEST}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#BAD_REQUEST}
	 */
	@ExceptionHandler({ConstraintViolationException.class})
	public ResponseEntity<CustomErrorResponse> handleEntityAlreadyExists(final HttpServletRequest request, final ConstraintViolationException exception) {
		return createResponseEntity(HttpStatus.BAD_REQUEST, request, exception);
	}
	
	/**
	 * Maps an {@link MiningAuthenticationException} to {@link HttpStatus#UNAUTHORIZED}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#UNAUTHORIZED}
	 */
	@ExceptionHandler({MiningAuthenticationException.class})
	public ResponseEntity<CustomErrorResponse> handleUnauthenticated(final HttpServletRequest request, final MiningAuthenticationException exception) {
		return createResponseEntity(HttpStatus.UNAUTHORIZED, request, exception);
	}
	
	/**
	 * Error handler for {@link AccessDeniedException}. Required to restore Spring's default behavior, as otherwise
	 * the default error handler is triggered and the status code is mapped to 500 instead of 403.
	 *
	 * @param exception the {@code AccessDeniedException}
	 * @return response entity with appropriate status code and reason phrase
	 */
	@ExceptionHandler(AccessDeniedException.class)
	public ResponseEntity<String> handleAccessDenied(final AccessDeniedException exception) {
		return new ResponseEntity<>(exception.getMessage(), HttpStatus.FORBIDDEN);
	}
	
	/**
	 * Maps an {@link RSQLParserException} to {@link HttpStatus#BAD_REQUEST}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#BAD_REQUEST}
	 */
	@ExceptionHandler(RSQLParserException.class)
	public ResponseEntity<CustomErrorResponse> handleRsqlParserException(final HttpServletRequest request, final RSQLParserException exception) {
		return createResponseEntity(HttpStatus.BAD_REQUEST, request, exception);
	}
	
	/**
	 * Maps an {@link IllegalArgumentException} to {@link HttpStatus#BAD_REQUEST}.
	 *
	 * @param request the request while the exception occurred
	 * @param exception the occurred exception
	 * @return response entity with {@link HttpStatus#BAD_REQUEST}
	 */
	@ExceptionHandler({IllegalArgumentException.class})
	public ResponseEntity<CustomErrorResponse> handleIllegalArgumentException(final HttpServletRequest request, final IllegalArgumentException exception) {
		return createResponseEntity(HttpStatus.BAD_REQUEST, request, exception);
	}
	
	private ResponseEntity<CustomErrorResponse> createResponseEntity(final HttpStatus status, final HttpServletRequest request, final Exception exception) {
		LOG.debug(() -> "Request " + request.getMethod() + " " + request.getRequestURI() + " failed", exception);
		
		final CustomErrorResponse response = new CustomErrorResponse();
		response.setTimestamp(LocalDateTime.now());
		response.setStatus(status.value());
		response.setError(exception.getClass().getName());
		response.setMessage(exception.getClass().getName() + " thrown from controller while trying to access " + request.getRequestURI());
		response.setPath(request.getRequestURL().toString());
		
		return new ResponseEntity<>(response, status);
	}
	
	private ResponseEntity<CustomErrorResponse> createResponseEntity(final HttpStatus status, final ResponseStatusException exception) {
		final CustomErrorResponse response = new CustomErrorResponse();
		response.setTimestamp(LocalDateTime.now());
		response.setStatus(status.value());
		response.setError(exception.getClass().getName());
		response.setMessage(exception.getMessage());
		return new ResponseEntity<>(response, status);
	}

	private ResponseEntity<CustomErrorResponse> createResponseEntity(final UserFacingException exception) {
		final CustomErrorResponse response = new CustomErrorResponse();
		response.setTimestamp(LocalDateTime.now());
		response.setStatus(exception.getStatus().value());
		response.setError(exception.getClass().getName());
		response.setMessage(exception.getMessage());
		return new ResponseEntity<>(response, exception.getStatus());
	}
}
