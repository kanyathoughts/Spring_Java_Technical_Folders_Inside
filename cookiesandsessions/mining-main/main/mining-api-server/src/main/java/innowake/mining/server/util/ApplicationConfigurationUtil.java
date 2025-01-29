/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.util;

import org.apache.commons.lang3.StringUtils;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.EnumerablePropertySource;

import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * Utility for dumping Spring's configuration environment to string
 * for debugging purposes.
 */
public class ApplicationConfigurationUtil {

    private ApplicationConfigurationUtil() {
        /* static utility class */
    }

    /**
     * Converts the properties in the application environment to string.
     * @param env the application environment
     * @return all properties in the environment as "key: value" pairs, one property per line
     */
    public static String getConfigAsString(final ConfigurableEnvironment env) {
        return StreamSupport.stream(env.getPropertySources().spliterator(), false)
                .filter(EnumerablePropertySource.class::isInstance)
                .flatMap(ps -> Arrays.stream(((EnumerablePropertySource<?>) ps).getPropertyNames()))
                .filter(prop -> ! StringUtils.containsAny(prop.toLowerCase(), "credentials", "password", "token", "key"))
                .distinct()
                .sorted()
                .map(prop -> prop + ": " + env.getProperty(prop))
                .collect(Collectors.joining("\n"));
    }
}
