package innowake.mining.data.debug.plugin;

import com.orientechnologies.common.log.OLogManager;
import com.orientechnologies.orient.server.OServer;
import com.orientechnologies.orient.server.config.OServerParameterConfiguration;
import com.orientechnologies.orient.server.plugin.OServerPluginAbstract;

import innowake.lib.core.api.lang.Nullable;

/**
 * Main entry point for the debug plugin.
 */
public class MiningDebugPlugin extends OServerPluginAbstract {

	private static final String PLUGIN_NAME = "innoWake mining data debug plugin";
	
	@Override
	public void config(@Nullable final OServer server, @Nullable final OServerParameterConfiguration[] params) {
		printConfiguration(params);
		super.config(server, params);
	}

	@Override
	public void startup() {
		OLogManager.instance().info(this, "Starting " + PLUGIN_NAME);

		super.startup();
	}

	@Override
	public void shutdown() {
	
		super.shutdown();
	}

	@Override
	public String getName() {
		return PLUGIN_NAME;
	}

	private void printConfiguration(@Nullable final OServerParameterConfiguration[] params) {
		if (params == null || params.length == 0) {
			OLogManager.instance().info(this, "No parameter configuration provided");
			return;
		}

		OLogManager.instance().info(this, "Parameter configuration provided:");
		for (final OServerParameterConfiguration configuration : params) {
			OLogManager.instance().info(this, String.format("%s: %s", configuration.name, configuration.value));
		}
	}


}