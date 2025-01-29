/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.sync;

import org.eclipse.core.resources.IProject;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Applies project configuration for the languages identified.
 */
public class ApplyProjectConfiguration {
	
	protected static final Logger LOG = LoggerFactory.getLogger(ApplyProjectConfiguration.class);
	
	private ApplyProjectConfiguration() {}
	
	/**
	 * Applies the project configuration.
	 *
	 * @param language the language for which the configuration is applied.
	 * @param project the project to apply.
	 */
	public static void apply(final Technology language, final IProject project) {
		final AbstractProjectConfigurator configurator;
		switch (language) {
			case COBOL:
				configurator = new CobolProjectConfigurator();
				break;
			case JCL:
				configurator = new JclProjectConfigurator();
				break;
			case NATURAL:
				configurator = new NaturalProjectConfiguration();
				break;
			case UNKNOWN:
			case NONE:
				return;
			default:
				try {
					configurator = new AbstractProjectConfigurator(ResolveTargetHelper.fromTechnology(language)) {};
				} catch (final IllegalArgumentException e) {
					LOG.error(() -> "Error applying configuration for the type: " + language + ". Root cause: " + e.getMessage());
					return;
				}
		}
		configurator.configure(project);
	}
}
