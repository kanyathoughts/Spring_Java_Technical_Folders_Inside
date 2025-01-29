/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Computes referenced data dictionaries for blocks of type {@link FunctionalBlockType#FUNCTIONAL_UNIT} and {@link FunctionalBlockType#FUNCTIONAL_GROUP}.
 */
@Component
public class ReferencedDataDictionaryComputation implements FunctionalBlockComputation<List<UUID>> {

	private final FunctionalBlockService functionalBlockService;
	private final DataDictionaryService dataDictionaryService;

	public ReferencedDataDictionaryComputation(final FunctionalBlockService functionalBlockService, final DataDictionaryService dataDictionaryService) {
		this.functionalBlockService = functionalBlockService;
		this.dataDictionaryService = dataDictionaryService;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		return FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.FUNCTIONAL_UNIT)
			|| FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.FUNCTIONAL_GROUP);
	}

	@Override
	public Map<FunctionalBlockPojo, List<UUID>> computeBatched(final List<FunctionalBlockPojo> functionalBlocks, final ProgressMonitor progressMonitor) {
		final Map<FunctionalBlockPojo, List<UUID>> result = new HashMap<>();
		if ( ! functionalBlocks.isEmpty()) {
			for (final FunctionalBlockPojo functionalBlock : functionalBlocks) {
				if (functionalBlock.isOfType(FunctionalBlockType.FUNCTIONAL_UNIT)) {
					result.put(functionalBlock, getDataDictionaryIdsFromFunctionalUnit(functionalBlock.getUid()));
				} else if (functionalBlock.isOfType(FunctionalBlockType.FUNCTIONAL_GROUP)) {
					final List<FunctionalBlockPojo> functionalUnits = functionalBlockService.findChildrenDeep(functionalBlock.getUid(), -1,
						q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
					final List<UUID> referencedDataDictionaries = new ArrayList<>();
					for (final FunctionalBlockPojo functionalUnit : functionalUnits) {
						referencedDataDictionaries.addAll(getDataDictionaryIdsFromFunctionalUnit(functionalUnit.getUid()));
					}
					result.put(functionalBlock, referencedDataDictionaries);
				}
			}
		}
		return result;
	}

	@Override
	public void persist(final UUID functionalBlockId, final List<UUID> dataDictionaryIds) {
		if ( dataDictionaryIds != null && ! dataDictionaryIds.isEmpty()) {
			functionalBlockService.setReferencedDataDictionaries(functionalBlockId, dataDictionaryIds);
		}
	}
	
	private List<UUID> getDataDictionaryIdsFromFunctionalUnit(final UUID functionalBlockUid) {
		final Optional<GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(functionalBlockUid);
		if (generatedFrom.isPresent() && generatedFrom.get().getAnnotationId().isPresent()) {
			return dataDictionaryService
				.find(q -> q.ofAnnotation(generatedFrom.get().getAnnotationId().get()))
				.stream().map(DataDictionaryPojo::getUid).collect(Collectors.toList());
		}
		return new ArrayList<>();
	}

}
