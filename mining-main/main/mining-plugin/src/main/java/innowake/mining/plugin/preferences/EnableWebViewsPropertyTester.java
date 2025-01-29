/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Optional;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IProject;
 
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Features;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.shared.model.FeatureId;
 
 
/**
 * Property Tester class to test Legacy Web views enabled or disabled
 */
public class EnableWebViewsPropertyTester extends PropertyTester {

	@Override
	public boolean test(
			@Nullable final Object receiver,
			@Nullable final String property,
			@Nullable final Object[] args,
			@Nullable final Object expectedValue) {
		if ("enableWebViews".equalsIgnoreCase(property)) {
			final Optional<IProject> project = SelectionUtil.getProjectFromSelectedResource();
			if (project.isPresent()) {
				return Features.INSTANCE.isEnabled(project.get(), FeatureId.LEGACY_WEB_VIEWS_IN_ECLIPSE);
			}
		}
		return false;
	}
}