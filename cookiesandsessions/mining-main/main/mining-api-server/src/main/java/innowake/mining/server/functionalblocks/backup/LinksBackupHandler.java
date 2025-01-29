/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup;

import com.fasterxml.jackson.core.type.TypeReference;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Handler backing up {@link FunctionalBlockLink}.
 */
@Component
public class LinksBackupHandler implements FunctionalBlockBackupHandler<List<FunctionalBlockLink>, List<FunctionalBlockLink>> {

	private final FunctionalBlockService functionalBlockService;

	public LinksBackupHandler(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	@Override
	public String getIdentifier() {
		return "links";
	}

	@Override
	public TypeReference<List<FunctionalBlockLink>> getInputType() {
		return new TypeReference<>() {};
	}

	@Override
	public Map<FunctionalBlockPojo, List<FunctionalBlockLink>> backup(final FunctionalBlockBackupContext context, final List<FunctionalBlockPojo> functionalBlocks) {
		final Map<FunctionalBlockPojo, List<FunctionalBlockLink>> ret = new HashMap<>(functionalBlocks.size());

		for (final FunctionalBlockPojo functionalBlock : functionalBlocks) {
			ret.put(functionalBlock, functionalBlockService.getLinks(functionalBlock.getUid()));
		}

		return ret;
	}

	@Override
	public void restore(final FunctionalBlockBackupContext context, final Map<UUID, List<FunctionalBlockLink>> inputData) {
		for (final Map.Entry<UUID, List<FunctionalBlockLink>> entry : inputData.entrySet()) {
			functionalBlockService.setLinks(entry.getKey(), entry.getValue());
		}
	}
}
