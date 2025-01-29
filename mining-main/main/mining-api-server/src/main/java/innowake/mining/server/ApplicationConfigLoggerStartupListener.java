/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.util.ApplicationConfigurationUtil;
import org.springframework.boot.context.event.ApplicationEnvironmentPreparedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.core.env.ConfigurableEnvironment;

/**
 * Listener for logging the application configuration as early as possible during startup, for debugging purposes.
 */
public class ApplicationConfigLoggerStartupListener implements ApplicationListener<ApplicationEnvironmentPreparedEvent> {

    private static final Logger APPLICATION_CONFIG_LOGGER = LoggerFactory.getLogger("innowake.mining.server.applicationconfig");

    @Override
    public void onApplicationEvent(final ApplicationEnvironmentPreparedEvent event) {
        logApplicationConfig(event.getEnvironment());
    }

    private void logApplicationConfig(final ConfigurableEnvironment env) {
        if (APPLICATION_CONFIG_LOGGER.isDebugEnabled()) {
            APPLICATION_CONFIG_LOGGER.debug(ApplicationConfigurationUtil.getConfigAsString(env));
        }
    }
}
