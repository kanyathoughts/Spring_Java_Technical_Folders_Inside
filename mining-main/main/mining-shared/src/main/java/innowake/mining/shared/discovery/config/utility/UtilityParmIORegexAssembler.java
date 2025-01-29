/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.mining.shared.discovery.config.core.Config;

public class UtilityParmIORegexAssembler {
	
	public static Optional<String> assembleInbound(final Config config, final String utilityName) {
		final Optional<UtilityEntity> entity = config.getUtilityList().findUtility(utilityName);
		if (entity.isPresent()) {
			final List<String> inbound = entity.get().getParameters().stream()
																     .map(UtilityParameter::getInbound)
																     .collect(Collectors.toList());
			if (inbound.isEmpty() || inbound.get(0).isEmpty()) {
				return Optional.empty();
			}
			return Optional.of(inbound.get(0));
		}
		return Optional.empty();
	}
	
	public static Optional<String> assembleOutbound(final Config config, final String utilityName) {
		final Optional<UtilityEntity> entity = config.getUtilityList().findUtility(utilityName);
		if(entity.isPresent()) {
			final List<String> outbound = entity.get().getParameters().stream()
																	  .map(UtilityParameter::getOutbound)
																      .map(UtilityOutbound::getOutBoundValue)
																	  .collect(Collectors.toList());
			if(outbound.isEmpty()) return Optional.empty();
			if(outbound.get(0).isEmpty()) return Optional.empty();
			return Optional.of(outbound.get(0));
		}
		return Optional.empty();
	}
}
