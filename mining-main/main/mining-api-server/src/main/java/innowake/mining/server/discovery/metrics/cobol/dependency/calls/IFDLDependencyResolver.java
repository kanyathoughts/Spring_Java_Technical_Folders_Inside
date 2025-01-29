/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol.dependency.calls;

import java.util.regex.Pattern;

import innowake.mining.shared.model.ModuleType;


/**
 * A dependency resolver for DECForms IFDLs.
 */
public class IFDLDependencyResolver extends CallDependencyResolver {
	private static final Pattern SCREEN_PATTERN = Pattern.compile("SCREENS:");
	private static final int RELEVANT_INDEX = 3;

	@Override
	protected String getTargetModuleName(final String stringWithArtifact) {
		final String afterScreens = SCREEN_PATTERN.split(stringWithArtifact)[1];
		return afterScreens.contains(".FORM") ? afterScreens.substring(0, afterScreens.indexOf(".FORM")) : afterScreens;
	}

	@Override
	protected int getRelevantIndex() {
		return RELEVANT_INDEX;
	}

	@Override
	protected boolean accept(final String target) {
		return target.equalsIgnoreCase("FORMS$ENABLE");
	}

	@Override
	protected ModuleType getTargetModuleType() {
		return ModuleType.IFDL_FORM;
	}

	@Override
	protected boolean filter(final String potentialName) {
		/* Accept every filename. */
		return true;
	}
}
