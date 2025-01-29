package innowake.mining.data.plugin;

import com.orientechnologies.orient.core.Orient;
import com.orientechnologies.orient.core.db.ODatabaseInternal;
import com.orientechnologies.orient.core.db.ODatabaseLifecycleListener;
import com.orientechnologies.orient.core.record.impl.ODocument;
import com.orientechnologies.orient.server.OServer;
import com.orientechnologies.orient.server.config.OServerParameterConfiguration;
import com.orientechnologies.orient.server.plugin.OServerPluginAbstract;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.plugin.additionalinfo.DeleteOrphanedAdditionalInfoHook;

/**
 * Main entry point for the data plugin.
 */
public class MiningPlugin extends OServerPluginAbstract implements ODatabaseLifecycleListener {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_DATA_PLUGIN);
	private static final String PLUGIN_NAME = "innoWake mining data plugin";
	private final DeleteOrphanedAdditionalInfoHook deleteOrphanedAdditionalInfoHook = new DeleteOrphanedAdditionalInfoHook();


	@Override
	public void config(@Nullable final OServer server, @Nullable final OServerParameterConfiguration[] params) {
		super.config(server, params);
		printConfiguration(params);
	}

	@Override
	public void startup() {
		super.startup();
		LOG.info(() -> "Starting " + PLUGIN_NAME);
		Orient.instance().addDbLifecycleListener(this);
	}


	@Override
	public String getName() {
		return PLUGIN_NAME;
	}


	private void printConfiguration(@Nullable final OServerParameterConfiguration[] params) {
		if (params == null || params.length == 0) {
			LOG.debug(() -> "No parameter configuration provided");
			return;
		}

		LOG.debug(() -> "Parameter configuration provided:");
		for (final OServerParameterConfiguration configuration : params) {
			LOG.debug(() -> String.format("%s: %s", configuration.name, configuration.value));
		}
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void onCreate(@Nullable final ODatabaseInternal iDatabase) {
		if (iDatabase == null) {
			return;
		}
		iDatabase.registerHook(deleteOrphanedAdditionalInfoHook);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void onOpen(@Nullable final ODatabaseInternal iDatabase) {
		if (iDatabase == null) {
			return;
		}
		iDatabase.registerHook(deleteOrphanedAdditionalInfoHook);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void onClose(@Nullable final ODatabaseInternal iDatabase) {
		if (iDatabase == null) {
			return;
		}
		iDatabase.unregisterHook(deleteOrphanedAdditionalInfoHook);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void onDrop(@Nullable final ODatabaseInternal iDatabase) {}

	@Override
	public void onLocalNodeConfigurationRequest(@Nullable final ODocument iConfiguration) {}
}
