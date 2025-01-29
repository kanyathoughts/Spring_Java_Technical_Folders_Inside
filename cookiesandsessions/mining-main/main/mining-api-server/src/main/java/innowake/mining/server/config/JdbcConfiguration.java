package innowake.mining.server.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.data.relational.core.dialect.AnsiDialect;
import org.springframework.data.relational.core.dialect.Dialect;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;

/**
 * Configures the JDBC Dialect. I don't think this has any effect, but newer Spring versions
 * refuse to autowire JdbcTemplate if the Dialect is not defined.
 */
@Configuration
@ConditionalOnProperty(name = "spring.datasource.enabled", matchIfMissing = true)
public class JdbcConfiguration extends AbstractJdbcConfiguration {

	@Override
	public Dialect jdbcDialect(final NamedParameterJdbcOperations operations) {
		return AnsiDialect.INSTANCE;
	}
}
