/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup;

import com.fasterxml.jackson.core.type.TypeReference;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * Handler backing up {@link GeneratedFrom}.
 */
@Component
public class GeneratedFromBackupHandler implements FunctionalBlockBackupHandler<GeneratedFrom, GeneratedFrom> {

	private final FunctionalBlockService functionalBlockService;

	public GeneratedFromBackupHandler(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	@Override
	public String getIdentifier() {
		return "generatedFrom";
	}

	@Override
	public TypeReference<GeneratedFrom> getInputType() {
		return new TypeReference<>() {};
	}

	@Override
	public Map<FunctionalBlockPojo, GeneratedFrom> backup(final FunctionalBlockBackupContext context, final List<FunctionalBlockPojo> functionalBlocks) {
		final Map<FunctionalBlockPojo, GeneratedFrom> ret = new HashMap<>(functionalBlocks.size());
		for (final FunctionalBlockPojo fb : functionalBlocks) {
			functionalBlockService.getGeneratedFrom(fb.getUid()).ifPresent(gf -> ret.put(fb, gf));
		}
		return ret;
	}

	@Override
	public void restore(final FunctionalBlockBackupContext context, final Map<UUID, GeneratedFrom> inputData) {
		for (final Map.Entry<UUID, GeneratedFrom> entry : inputData.entrySet()) {
			final GeneratedFrom generatedFrom = entry.getValue();
			final Optional<EntityId> annotationId = generatedFrom.getAnnotationId();
			if (annotationId.isPresent() && annotationId.get().hasNid()) {
				final EntityId newAnnotationId = context.getRestoredAnnotationIdMap().get(annotationId.get().getNid());
				if (newAnnotationId != null) {
					functionalBlockService.setGeneratedFrom(entry.getKey(), GeneratedFrom.fromAnnotation(newAnnotationId));
					continue;
				}
			}
			functionalBlockService.setGeneratedFrom(entry.getKey(), generatedFrom);
		}
	}
}
