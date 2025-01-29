package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import org.apache.commons.lang3.StringUtils;

import java.util.Optional;

/**
 * Utility class for safely executing functions that might throw exceptions.
 */
public class SafeExecUtils {

	private static final Logger LOG = LoggerFactory.getLogger(SafeExecUtils.class);

	@FunctionalInterface
	public interface CheckedFunction<T, R> {
		R apply(T t) throws Exception;
	}

	public interface CheckedSupplier<R> {
		R apply() throws Exception;
	}

	/**
	 * Execute a function and catch any exception that might be thrown. If an exception is caught, an error is added to the moduleBuilder.
	 * @param function the function to execute
	 * @param param the parameter to pass to the function
	 * @param moduleBuilder the moduleBuilder to add errors to
	 * @return an Optional containing the result of the function, or an empty Optional if an exception was caught
	 * @param <T> the type of the parameter
	 * @param <R> the type of the result
	 */
	public static <T, R> Optional<R> safeExecute(final CheckedFunction<T, R> function, final T param, final DiscoveryBuilder.ModuleBuilder moduleBuilder) {
		return safeExecute(function, param, moduleBuilder, ErrorKey.MODULE_ABORT, Optional.empty());
	}

	/**
	 * Execute a function and catch any exception that might be thrown. If an exception is caught, an error is added to the moduleBuilder.
	 *
	 * @param function the function to execute
	 * @param moduleBuilder the moduleBuilder to add errors to
	 * @return an Optional containing the result of the function, or an empty Optional if an exception was caught
	 * @param <R> the type of the result
	 */
	public static <R> Optional<R> safeExecute(final CheckedSupplier<R> function, final DiscoveryBuilder.ModuleBuilder moduleBuilder) {
		return safeExecute(function, moduleBuilder, ErrorKey.MODULE_ABORT, Optional.empty());
	}


	/**
	 * Execute a function and catch any exception that might be thrown. If an exception is caught, an error is added to the moduleBuilder.
	 *
	 * @param function the function to execute
	 * @param param the parameter to pass to the function
	 * @param moduleBuilder the moduleBuilder to add errors to
	 * @param errorKey the error key to use if an exception is caught
	 * @param errorMessage the error message to use if an exception is caught
	 * @return an Optional containing the result of the function, or an empty Optional if an exception was caught
	 * @param <T> the type of the parameter
	 * @param <R> the type of the result
	 */
	public static <T, R> Optional<R> safeExecute(final CheckedFunction<T, R> function, final T param, final DiscoveryBuilder.ModuleBuilder moduleBuilder,
			final ErrorKey errorKey, final Optional<String> errorMessage) {
		try {
			return Optional.of(function.apply(param));
		} catch (final Exception e) {
			LOG.error("Error occurred while executing function: " + errorMessage.orElse(StringUtils.EMPTY), e);
			moduleBuilder.addError(Severity.ERROR, errorKey, errorMessage.orElse(e.getMessage()));
		} catch (final Throwable e) {
			LOG.error("Runtime Error occurred while executing function: " + errorMessage.orElse(StringUtils.EMPTY), e);
			moduleBuilder.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}
		return Optional.empty();
	}

	/**
	 * Execute a function and catch any exception that might be thrown. If an exception is caught, an error is added to the moduleBuilder.
	 *
	 * @param function the function to execute
	 * @param moduleBuilder the moduleBuilder to add errors to
	 * @param errorKey the error key to use if an exception is caught
	 * @param errorMessage the error message to use if an exception is caught
	 * @return an Optional containing the result of the function, or an empty Optional if an exception was caught
	 * @param <R> the type of the result
	 */
	public static <R> Optional<R> safeExecute(final CheckedSupplier<R> function, final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ErrorKey errorKey,
			final Optional<String> errorMessage) {
		try {
			return Optional.of(function.apply());
		} catch (final Exception e) {
			LOG.error("Error occurred while executing function: " + errorMessage.orElse(StringUtils.EMPTY), e);
			moduleBuilder.addError(Severity.ERROR, errorKey, errorMessage.orElse(e.getMessage()));
		} catch (final Throwable e) {
			LOG.error("Runtime Error occurred while executing function: " + errorMessage.orElse(StringUtils.EMPTY), e);
			moduleBuilder.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}
		return Optional.empty();
	}
}
