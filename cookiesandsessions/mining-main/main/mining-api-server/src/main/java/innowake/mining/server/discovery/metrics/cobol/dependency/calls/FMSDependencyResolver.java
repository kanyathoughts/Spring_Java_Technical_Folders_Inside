/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol.dependency.calls;

import innowake.mining.shared.model.ModuleType;

/**
 * A dependency resolver for FMS maps.
 */
public class FMSDependencyResolver extends CallDependencyResolver {
	private static final int RELEVANT_INDEX = 0;

	@Override
	protected String getTargetModuleName(final String stringWithArtifact) {
		return stringWithArtifact.replace("\"", "").toUpperCase();
	}

	@Override
	protected int getRelevantIndex() {
		return RELEVANT_INDEX;
	}

	@Override
	protected boolean accept(final String call) {
		final String upperCall = call.toUpperCase();
		return upperCall.equals("FDV$CLRSH") || upperCall.equals("FDV$CDISP") || upperCall.equals("FDV$SHOW") || upperCall.equals("FDV$DISP");
	}

	@Override
	protected ModuleType getTargetModuleType() {
		return ModuleType.FMS_FORM;
	}

	@Override
	protected boolean filter(final String potentialName) {
		/* Acccept every fileName. */
		return true;
	}
}
