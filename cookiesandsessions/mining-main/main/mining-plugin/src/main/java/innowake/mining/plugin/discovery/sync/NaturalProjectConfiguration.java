/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.discovery.sync;

import java.util.Properties;

import org.eclipse.core.resources.IProject;

import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.natclipse.core.NatclipseCore;

/**
 * Natural language project configuration.
 * <li>Set the natclipse project nature.
 * <li>Copy the initial .nat-path file into the project root.
 */
public class NaturalProjectConfiguration extends AbstractProjectConfigurator {

	private static final String NATCLIPSE_PROPERTIES_FILE_NAME = ".settings/innowake.natclipse.prefs";

	public NaturalProjectConfiguration() {
		super(ResolveTarget.NATURAL);
	}

	@Override
	public void configure(final IProject project) {
		try {
			addProjectNature(NatclipseCore.NATURE, project);
			copy(".nat-path", project);

			final Properties props = loadProperties(project, NATCLIPSE_PROPERTIES_FILE_NAME);
			props.putIfAbsent("eclipse.preferences.version", "1");

			/* enables dependency lookup in complete Natural source folder */
			props.put("natural.properties.devmode.projectspecificsettings", "true");
			props.put("natural.devmode.steplibusage", "none");

			/* enforces data area normalization which is presumed for dependency lookup */
			props.put("natural.properties.projectspecificsettings", "true");
			props.remove("natural.normalizemode");

			saveProperties(project, NATCLIPSE_PROPERTIES_FILE_NAME, props);

			super.configure(project);

		} catch (final Exception e) {
			LOG.error(() -> "Error while applying Natural configurator: " + e);
		}
	}

}
