/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.config;

import javax.persistence.EntityNotFoundException;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.graphql.execution.DataFetcherExceptionResolver;
import org.springframework.graphql.execution.DataFetcherExceptionResolverAdapter;
import org.springframework.graphql.execution.ErrorType;
import org.springframework.security.access.AccessDeniedException;

import graphql.GraphQLError;
import graphql.GraphqlErrorException;
import graphql.schema.DataFetchingEnvironment;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.Logging;

/**
 * Registers error handling beans for GraphQL. These are injected into {@link MiningGraphQlSource}.
 */
@Configuration
public class MiningGraphQlErrorHandlerConfiguration {

	private static final Logger LOG_GRAPHQL = LoggerFactory.getLogger(Logging.GRAPHQL);
	
	/**
	 * Provides an "exception resolver" which doesn't actually resolve exceptions but only outputs them to the server log.
	 * 
	 * @return an exception resolver that logs exceptions
	 */
	@Bean
	public DataFetcherExceptionResolver errorLoggingExceptionResolver() {
		return new DataFetcherExceptionResolverAdapter() {
			@Override
			@Nullable
			protected GraphQLError resolveToSingleError(final Throwable ex, final DataFetchingEnvironment env) {
				LOG_GRAPHQL.error(() -> "Error while handling GraphQl request:", ex);
				if (ex instanceof EntityNotFoundException || ex instanceof AccessDeniedException) {
					return null;
				}
				return new GraphqlErrorException.Builder()
						.errorClassification(ErrorType.INTERNAL_ERROR)
						.message(ex.getMessage())
						.cause(ex)
						.build();
			}
		};
	}
	
	/**
	 * 
	 * Provides an "exception resolver" for queries not returning any records.
	 *
	 * @return an exception resolver for NoRecordFoundException
	 */
	@Bean
	public DataFetcherExceptionResolver noRecordFoundExceptionResolver() {
		return new DataFetcherExceptionResolverAdapter() {
			@Override
			@Nullable
			protected GraphQLError resolveToSingleError(final Throwable ex, final DataFetchingEnvironment env) {
				if (ex instanceof EntityNotFoundException) {
					return new GraphqlErrorException.Builder()
							.errorClassification(ErrorType.NOT_FOUND)
							.message("No record found")
							.cause(ex)
							.build();
				}
				return null;
			}
		};
	}

}
