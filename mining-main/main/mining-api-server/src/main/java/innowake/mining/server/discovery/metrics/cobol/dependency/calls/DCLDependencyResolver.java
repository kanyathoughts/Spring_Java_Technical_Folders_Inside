/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol.dependency.calls;

import java.util.regex.Pattern;

import innowake.mining.shared.model.ModuleType;


/**
 * A dependency resolver for COBOL -> DCL calls.
 */
public class DCLDependencyResolver extends CallDependencyResolver {
	private static final Pattern COM_PATTERN = Pattern.compile("COM:");
	private static final int RELEVANT_DCL_INDEX = 0;

	@Override
	protected String getTargetModuleName(final String stringWithArtifact) {
		final String afterCOM = COM_PATTERN.split(stringWithArtifact)[1];
		return afterCOM.contains(".COM") ? afterCOM.substring(0, afterCOM.indexOf(".COM")) : afterCOM;
	}

	@Override
	protected int getRelevantIndex() {
		return RELEVANT_DCL_INDEX;
	}

	@Override
	protected boolean accept(final String target) {
		return target.equalsIgnoreCase("LIB$SPAWN");
	}

	@Override
	protected ModuleType getTargetModuleType() {
		return ModuleType.DCL;
	}
	
	@Override
	protected boolean filter(final String potentialName) {
		return potentialName.contains("COM:");
	}

}
