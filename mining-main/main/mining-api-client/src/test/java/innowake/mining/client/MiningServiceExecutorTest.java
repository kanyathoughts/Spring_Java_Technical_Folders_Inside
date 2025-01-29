/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.util.Optional;
import java.util.function.Consumer;

import org.junit.Test;

import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;

/**
 * Tests for mining service executor to execute service calls.
 * Following are the prerequisites to run the test case :-
 * a) The api-server should be running in authorized profile in order to be able to fetch the keycloak configuration.
 * b) The keycloak server should be running.
 * c) The method level annotation @Ignore should be removed before running the test cases.
 * d) The test case should be run using JUnit 4 test runner.
 */
public class MiningServiceExecutorTest {

	/**
	 * Test for obtaining the {@link Result#getValue()} if result is valid on execution of the service call.
	 * Also verifies validResultConsumer is executed.
	 *
	 * @throws IOException see {@link RestService#execute()}
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testExecuteWithValidResult() throws IOException {
		final RestService<String> restServiceMock = mock(RestService.class);
		final Result<String> resultMock = mock(Result.class);
		given(restServiceMock.execute()).willReturn(resultMock);
		given(Boolean.valueOf(resultMock.isValid())).willReturn(Boolean.TRUE);
		final Optional<String> expectedValue = Optional.of("DummyExpectedValue");
		given(resultMock.getValue()).willReturn(expectedValue);
		final Consumer<String> validResultConsumerMock = mock(Consumer.class);
		assertEquals(
				MiningServiceExecutor
					.create(() -> restServiceMock)
					.setValidResultConsumer(validResultConsumerMock)
					.execute(), 
				expectedValue);
		verify(validResultConsumerMock, times(1)).accept(expectedValue.get());
	}
	
	/**
	 * Test for obtaining the empty optional if result is invalid on execution of the service call.
	 * Also verifies inValidResultConsumer is executed.
	 *
	 * @throws IOException see {@link RestService#execute()}
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testExecuteWithInvalidResult() throws IOException {
		final RestService<String> restServiceMock = mock(RestService.class);
		final Result<String> resultMock = mock(Result.class);
		given(restServiceMock.execute()).willReturn(resultMock);
		given(Boolean.valueOf(resultMock.isValid())).willReturn(Boolean.FALSE);
		final Consumer<Result<String>> inValidResultConsumerMock = mock(Consumer.class);
		assertFalse(
				MiningServiceExecutor
					.create(() -> restServiceMock)
					.setInvalidResultConsumer(inValidResultConsumerMock)
					.execute().isPresent());
		verify(inValidResultConsumerMock, times(1)).accept(resultMock);
	}

	/**
	 * Test for obtaining the empty optional if ioException occurred on execution of the service call.
	 * Also verifies exceptionConsumer is executed.
	 *
	 * @throws IOException see {@link RestService#execute()}
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testShouldExecuteExceptionConsumerOnException() throws IOException {
		final RestService<String> restServiceMock = mock(RestService.class);
		final Exception ioExceptionMock = mock(IOException.class);
		given(restServiceMock.execute()).willThrow(ioExceptionMock);
		final Consumer<Exception> exceptionConsumerMock = mock(Consumer.class);
		assertFalse(
				MiningServiceExecutor.create(() -> restServiceMock)
					.setExceptionConsumer(exceptionConsumerMock)
					.execute()
					.isPresent());
		verify(exceptionConsumerMock, times(1)).accept(ioExceptionMock);
	}

	/**
	 * Test to check {@link ExecutionException} is thrown if checked exception occurs and exception consumer is not 
	 * provided to {@link MiningServiceExecutor}.
	 *
	 * @throws IOException see {@link RestService#execute()}
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testShouldThrowExecutionException() throws IOException {
		final RestService<String> restServiceMock = mock(RestService.class);
		final Exception ioExceptionMock = mock(IOException.class);
		given(restServiceMock.execute()).willThrow(ioExceptionMock);
		ExecutionException exception = assertThrows(ExecutionException.class, () -> MiningServiceExecutor.create(() -> restServiceMock).execute());
		assertSame(ioExceptionMock, exception.getCause());
	}

	/**
	 * Test for obtaining the {@link Result} object on executing the service call.
	 *
	 * @param <T> the type of the service result
	 * @throws IOException see {@link RestService#execute()}
	 */
	@SuppressWarnings("unchecked")
	@Test
	public <T> void testGetResult() throws IOException {
		final RestService<T> restServiceMock = mock(RestService.class);
		final Result<T> resultMock = mock(Result.class);
		given(restServiceMock.execute()).willReturn(resultMock);
		given(Boolean.valueOf(resultMock.isValid())).willReturn(Boolean.TRUE);
		assertEquals(MiningServiceExecutor.create(() -> restServiceMock).getResult(), resultMock);
	}

}
