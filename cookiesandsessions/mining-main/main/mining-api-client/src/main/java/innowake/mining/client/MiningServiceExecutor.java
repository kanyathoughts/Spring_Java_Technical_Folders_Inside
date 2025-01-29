/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client;

import java.util.Optional;
import java.util.function.Consumer;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.client.exceptions.StaleTokenException;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.shared.lang.SupplierWithException;
import innowake.mining.shared.model.Entity;

/**
 * Executes a {@link RestService} and provides options to process the result and to configure error handling.
 * <br>
 * <pre>
 * MiningServiceExecutor.create(this::findModule)
 * 	.setValidResultConsumer(this::processModule)
 * 	.setInvalidResultConsumer(this::handleInvalidResult)
 * 	.setExceptionConsumer(this::handleException)
 * 	.execute();
 * </pre>
 * 
 * @param <T> the type of the {@link Entity}
 */
public class MiningServiceExecutor<T> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT);
	
	private final SupplierWithException<? extends RestService<T>, Exception> service;

	private Optional<Consumer<T>> validResultConsumer = Optional.empty();
	private Optional<Consumer<Result<T>>> invalidResultConsumer = Optional.empty();
	private Optional<Consumer<Exception>> exceptionConsumer = Optional.empty();
	private Optional<Runnable> additionalErrorHandler = Optional.empty();
	
	/**
	 * Creates a new instance.
	 *
	 * @param service supplier of a {@link RestService} to execute
	 * @return a new {@code ServiceExecutor} instance
	 */
	public static <T> MiningServiceExecutor<T> create(final SupplierWithException<? extends RestService<T>, Exception> service) {
		return new MiningServiceExecutor<>(service);
	}
	
	private MiningServiceExecutor(final SupplierWithException<? extends RestService<T>, Exception> service) {
		this.service = service;
	}
	
	/**
	 * Sets the consumer of the valid result after the {@link RestService} was executed.
	 *
	 * @param validResultConsumer consumer of the valid result
	 * @return this instance for method chaining
	 */
	public MiningServiceExecutor<T> setValidResultConsumer(final Consumer<T> validResultConsumer) {
		this.validResultConsumer = Optional.of(validResultConsumer);
		return this;
	}
	
	/**
	 * Sets the consumer of the invalid result after the {@link RestService} was executed.
	 *
	 * @param invalidResultConsumer consumer of the invalid result
	 * @return this instance for method chaining
	 */
	public MiningServiceExecutor<T> setInvalidResultConsumer(final Consumer<Result<T>> invalidResultConsumer) {
		this.invalidResultConsumer = Optional.of(invalidResultConsumer);
		return this;
	}
	
	/**
	 * Sets the consumer of the exception in case one was thrown while the {@link RestService} was executed.
	 * If exception consumer is not specified, executing the service call throws a runtime {@link ExecutionException} with the cause
	 * set as the occurred checked exception. Otherwise, the exception consumer must handle the occurred checked exception.
	 *
	 * @param exceptionConsumer consumer of the exception
	 * @return this instance for method chaining
	 */
	public MiningServiceExecutor<T> setExceptionConsumer(final Consumer<Exception> exceptionConsumer) {
		this.exceptionConsumer = Optional.of(exceptionConsumer);
		return this;
	}
	
	/**
	 * Sets an additional handler in case the result is invalid or an exception was thrown while the {@link RestService} was executed.
	 * <br>
	 * The additionalErrorHandler is skipped if either
	 * <br> a) runtime exception is thrown from service execution or from any of the consumer, or 
	 * <br> b) checked exception is thrown from execution of rest service and exceptionConsumer is not present resulting in {@link ExecutionException}.
	 * 
	 * @param additionalErrorHandler an additional error handler
	 * @return this instance for method chaining
	 */
	public MiningServiceExecutor<T> setAdditionalErrorHandler(final Runnable additionalErrorHandler) {
		this.additionalErrorHandler = Optional.of(additionalErrorHandler);
		return this;
	}

	/**
	 * Executes the {@link RestService} supplied by the {@link SupplierWithException}, invokes the given {@linkplain Consumer validResultConsumer}, if any, 
	 * and returns a {@link Result} with the result of the service or null in case of an error or invalid result.
	 * See {@link Result#isValid()}.
	 * <br>
	 * <br>
	 * In case the service call could not be executed if {@link StaleTokenException} occurred due to invalid_grant,
	 * it triggers a re-login with keycloak using the desktop variant and retries the service call.
	 * <br>
	 * The retry is only done once. Any further occurrence of {@link StaleTokenException} is thrown to the caller of MiningServiceExecutor
	 * as it may be due to other possible causes of invalid_grant other than user session being expired or revoked.
	 * Refer to <a href="https://tools.ietf.org/html/rfc6749#section-5.2">RFC 6749 - OAuth 2.0 documentation.</a>
	 * <br>
	 * <br>
	 * Before returning the result, it is checked for validity. If {@link Result#isValid()} returns {@code false} the given 
	 * {@linkplain Consumer invalidResultConsumer}, if any, followed by the given {@linkplain Runnable additionalErrorHandler}, if any, is invoked.  
	 * <br>
	 * If the executed {@link RestService} throws checked exception the given {@linkplain Consumer exceptionConsumer}, if any, 
	 * followed by the given {@linkplain Runnable additionalErrorHandler}, if any, is invoked.
	 *
	 * @return the service {@link Result} or {@code null}
	 * @throws ExecutionException which is a runtime exception thrown specifically with the cause
	 * set as the checked exception if exceptionConsumer is absent
	 * @throws StaleTokenException which is a runtime exception thrown if the execution of service failed twice due to invalid_grant
	 * @throws RuntimeException any other runtime exception if occurred while executing the service call or from
	 * validResultConsumer / invalidResultConsumer / exceptionConsumer / additionalErrorHandler is also thrown
	 */
	@Nullable
	public Result<T> getResult() {
		try {
			final Optional<Result<T>> resultOptional = callRestService(service.get());
			if (resultOptional.isPresent()) {
				final Result<T> result = resultOptional.get();
				if (result.isValid()) {
					validResultConsumer.ifPresent(consumer -> consumer.accept(result.getValue().orElse(null)));
					return result;
				}
				
				invalidResultConsumer.ifPresent(consumer -> consumer.accept(result));
			}
		} catch (final RuntimeException runtimeException) {
			throw runtimeException;
		} catch (final Exception exception) {
			/* Generic block for handling checked exceptions. If exception consumer is not specified, it throws a runtime ExecutionException
			 * with the cause set as the occurred checked exception. Otherwise, the exception consumer must handle the occurred checked exception. */
			if ( ! exceptionConsumer.isPresent()) {
				additionalErrorHandler.ifPresent(Runnable::run);
				throw new ExecutionException(exception);
			}
			exceptionConsumer.get().accept(exception);
		}
		
		additionalErrorHandler.ifPresent(Runnable::run);
		return null;
	}
	
	/**
	 * Executes the {@link RestService} supplied by the {@link SupplierWithException}, invokes the given {@linkplain Consumer validResultConsumer}, if any, 
	 * and returns an {@link Optional} with the result of the service or an empty {@link Optional} in case of an error or invalid result.
	 * See {@link Result#isValid()}.
	 * <br>
	 * <br>
	 * In case the service call could not be executed if {@link StaleTokenException} occurred due to invalid_grant,
	 * it triggers a re-login with keycloak using the desktop variant and retries the service call.
	 * <br>
	 * The retry is only done once. Any further occurrence of {@link StaleTokenException} is thrown to the caller of MiningServiceExecutor
	 * as it may be due to other possible causes of invalid_grant other than user session being expired or revoked.
	 * Refer to <a href="https://tools.ietf.org/html/rfc6749#section-5.2">RFC 6749 - OAuth 2.0 documentation.</a>
	 * <br>
	 * <br>
	 * Before returning the result, it is checked for validity. If {@link Result#isValid()} returns {@code false} the given 
	 * {@linkplain Consumer invalidResultConsumer}, if any, followed by the given {@linkplain Runnable additionalErrorHandler}, if any, is invoked.  
	 * <br>
	 * If the executed {@link RestService} throws checked exception the given {@linkplain Consumer exceptionConsumer}, if any, 
	 * followed by the given {@linkplain Runnable additionalErrorHandler}, if any, is invoked.
	 *
	 * @return the service result as {@link Optional} or an empty {@link Optional}
	 * @throws ExecutionException which is a runtime exception thrown specifically with the cause
	 * set as the checked exception if exceptionConsumer is absent
	 * @throws StaleTokenException which is a runtime exception thrown if the execution of service failed twice due to invalid_grant
	 * @throws RuntimeException any other runtime exception if occurred while executing the service call or from
	 * validResultConsumer / invalidResultConsumer / exceptionConsumer / additionalErrorHandler is also thrown
	 */
	public Optional<T> execute() {
		final Result<T> result = getResult();
		if (result != null) {
			return result.getValue();
		}
		return Optional.empty();
	}
	
	/**
	 * Executes the {@link RestService} supplied by the {@link SupplierWithException}, invokes the given {@linkplain Consumer validResultConsumer}, if any, 
	 * and returns an {@link Optional} with the result of the service or an empty {@link Optional} in case of an error or invalid result.
	 * See {@link Result#isValid()}.
	 * <br>
	 * <br>
	 * In case the service call could not be executed if {@link StaleTokenException} occurred due to invalid_grant,
	 * it triggers a re-login with keycloak using the desktop variant and retries the service call.
	 * <br>
	 * The retry is only done once. Any further occurrence of {@link StaleTokenException} is thrown to the caller of MiningServiceExecutor
	 * as it may be due to other possible causes of invalid_grant other than user session being expired or revoked.
	 * Refer to <a href="https://tools.ietf.org/html/rfc6749#section-5.2">RFC 6749 - OAuth 2.0 documentation.</a>
	 * <br>
	 * <br>
	 * Before returning the result, it is checked for validity. If {@link Result#isValid()} returns {@code false} the {@link Result#getExtendedStatusMessage()}
	 * is logged and the given {@linkplain Runnable additionalErrorHandler}, if any, is invoked.  
	 * <br>
	 * If the executed {@link RestService} throws checked exception it is logged and the given {@linkplain Runnable additionalErrorHandler}, if any, is invoked.
	 *
	 * @return the service result as {@link Optional} or an ampty {@link Optional}
	 * @throws StaleTokenException which is a runtime exception thrown if the execution of service failed twice due to invalid_grant
	 * @throws RuntimeException any other runtime exception if occurred while executing the service call or from
	 * validResultConsumer / invalidResultConsumer / exceptionConsumer / additionalErrorHandler is also thrown
	 */
	public Optional<T> executeWithDefaultErrorHandling() {
		setInvalidResultConsumer(result -> LOG.error(() -> result.getExtendedStatusMessage()));
		setExceptionConsumer(exception -> LOG.error(() -> exception.getLocalizedMessage(), exception));
		return execute();
	}
	
	private Optional<Result<T>> callRestService(final RestService<T> restService) throws Exception {
		return Optional.of(restService.execute());
	}

}
