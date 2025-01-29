/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup;

import brave.Tracer;
import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.server.functionalblocks.backup.model.FunctionalBlockAdditionalData;
import innowake.mining.server.functionalblocks.backup.model.FunctionalBlockBackup;
import innowake.mining.server.functionalblocks.service.FunctionalBlockOperationsService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Service for creating a list of {@link FunctionalBlockBackup} from a functional block query and
 * restoring functional blocks from a list of {@link FunctionalBlockBackup}.
 * @see innowake.mining.extensions.metadata.model.MetaDataBackup
 */
@Service
public class FunctionalBlockBackupService {

	private static final Logger LOG = LoggerFactory.getLogger(FunctionalBlockBackupService.class);

	private final FunctionalBlockService functionalBlockService;
	private final List<FunctionalBlockBackupHandler<?, ?>> functionalBlockBackupHandlers;
	private final ObjectMapper objectMapper;
	private final FunctionalBlockOperationsService functionalBlockOperationsService;
	private final Tracer tracer;

	public FunctionalBlockBackupService(final FunctionalBlockService functionalBlockService,
										final List<FunctionalBlockBackupHandler<?, ?>> functionalBlockBackupHandlers,
										final ObjectMapper objectMapper,
										final FunctionalBlockOperationsService functionalBlockOperationsService,
										final Tracer tracer) {
		this.functionalBlockService = functionalBlockService;
		this.functionalBlockBackupHandlers = functionalBlockBackupHandlers;
		this.objectMapper = objectMapper;
		this.functionalBlockOperationsService = functionalBlockOperationsService;
		this.tracer = tracer;
	}

	/**
	 * Creates a list of {@link FunctionalBlockBackup} containing the data from the functional blocks that match the query defined by the given
	 * {@code inquiryBuilder}.
	 *
	 * @param inquiryBuilder configures a {@link FunctionalBlockInquiryBuilder} to match a number of functional blocks
	 * @return the matching functional blocks in a format that can be backed up
	 */
	public List<FunctionalBlockBackup> createFunctionalBlockBackup(final BuildingConsumer<FunctionalBlockInquiryBuilder> inquiryBuilder) {
		final List<FunctionalBlockPojo> functionalBlocks = functionalBlockService.find(inquiryBuilder);
		final Map<String, Map<FunctionalBlockPojo, ?>> additionalData = new HashMap<>();
		final FunctionalBlockBackupContext context = new FunctionalBlockBackupContext(Collections.emptyMap());
		for (final FunctionalBlockBackupHandler<?, ?> handler : functionalBlockBackupHandlers) {
			final Map<FunctionalBlockPojo, ?> backup = handler.backup(context, functionalBlocks);
			additionalData.put(handler.getIdentifier(), backup);
		}
		return functionalBlocks.stream()
				.map(fb -> new FunctionalBlockBackup(fb, additionalData.entrySet().stream()
							.filter(data -> data.getValue().containsKey(fb))
							.map(data -> new FunctionalBlockAdditionalData(data.getKey(), data.getValue().get(fb)))
							.collect(Collectors.toList())))
				.collect(Collectors.toList());
	}

	/**
	 * Restores (i.e. inserts into the database) the data contained in the given list of {@link FunctionalBlockBackup}.
	 * Existing blocks are overwritten if they have the same uid as blocks in the backup.
	 *
	 * @param backups list of backed up data for functional blocks
	 * @param restoredAnnotationIdMap map of Annotation ids:
	 * key is the id of the Annotations as stored in the backup, value is the id of the Annotation after restore
	 */
	public void restoreFunctionalBlockBackup(final EntityId projectId, final List<FunctionalBlockBackup> backups, final Map<Long, EntityId> restoredAnnotationIdMap) {
		final Map<String, FunctionalBlockBackupHandler<?, ?>> handlersByType = functionalBlockBackupHandlers.stream()
				.collect(Collectors.toMap(FunctionalBlockBackupHandler::getIdentifier, Function.identity()));
		final Map<String, Map<UUID, Object>> additionalDataMap = new HashMap<>();

		/* restore functional blocks without children and collect additional data in map */
		for (final FunctionalBlockBackup backup : backups) {
			final FunctionalBlockPojoPrototype prototype = objectMapper.convertValue(backup.getFunctionalBlock(), FunctionalBlockPojoPrototype.class);
			prototype.setProject(projectId);
			/* children can be restored only after all blocks have been created */
			prototype.children.unset();
			final UUID functionalBlockUid = functionalBlockService.create(prototype);
			for (final FunctionalBlockAdditionalData additionalData : backup.getAdditionalData()) {
				additionalDataMap.computeIfAbsent(additionalData.getType(), k -> new HashMap<>()).put(functionalBlockUid, additionalData);
			}
		}

		/* restore children after all blocks have been restored */
		for (final FunctionalBlockBackup backup : backups) {
			final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype();
			prototype.setUid(backup.getFunctionalBlock().getUid());
			prototype.setChildren(backup.getFunctionalBlock().getChildren());
			functionalBlockService.update(prototype);
		}

		/* invoke handler to restore additional data collected in map */
		for (final Map.Entry<String, Map<UUID, Object>> entry : additionalDataMap.entrySet()) {
			final FunctionalBlockBackupHandler<?, ?> handler = handlersByType.get(entry.getKey());
			if (handler == null) {
				LOG.warn("Unable to restore additional data of type " + entry.getKey() + " for functional blocks: no handler for this data type");
				continue;
			}
			restoreAdditionalData(handler, entry.getValue(), restoredAnnotationIdMap);
		}

		/* execute functional block computation to restore any cached and computed information for the restored functional blocks
		 * execute it first on MODULE blocks and then on all other blocks, as the computation for other blocks may require MODULE blocks
		 * to already be up-to-date */
		final Set<UUID> moduleBlocks = backups.stream()
				.filter(backup -> FunctionalBlockUtil.hasType(backup.getFunctionalBlock(), FunctionalBlockType.MODULE))
				.map(backup -> backup.getFunctionalBlock().getUid())
				.collect(Collectors.toSet());
		final Set<UUID> singleReachabilityBlocks = backups.stream()
				.filter(backup -> ! FunctionalBlockUtil.hasType(backup.getFunctionalBlock(), List.of(FunctionalBlockType.MODULE,
								FunctionalBlockType.MERGE_PARENT)))
				.map(backup -> backup.getFunctionalBlock().getUid())
				.collect(Collectors.toSet());
		final Set<UUID> mergedReachabilityBlock = backups.stream()
				.filter(backup -> FunctionalBlockUtil.hasType(backup.getFunctionalBlock(), FunctionalBlockType.MERGE_PARENT))
				.map(backup -> backup.getFunctionalBlock().getUid())
				.collect(Collectors.toSet());
		functionalBlockOperationsService.executeFunctionalBlockComputation(moduleBlocks, new JobExecutionCallback() {
			@Override
			public void onCompletion() {
				try (final Tracer.SpanInScope parentScope = tracer.withSpanInScope(tracer.newTrace())) {
					functionalBlockOperationsService.executeFunctionalBlockComputation(singleReachabilityBlocks, new JobExecutionCallback() {

						@Override
						public void onCompletion() {
							try (final Tracer.SpanInScope childScope = tracer.withSpanInScope(tracer.newTrace())) {
								functionalBlockOperationsService.executeFunctionalBlockComputation(mergedReachabilityBlock);
							}
						}

						@Override
						public void onFailure(@Nullable final Throwable throwable) {
							LOG.error("Failed to execute functional block computation after restoring backup", throwable);
						}
					});
				}
			}

			@Override
			public void onFailure(@Nullable final Throwable throwable) {
				LOG.error("Failed to execute functional block computation after restoring backup", throwable);
			}
		});
	}

	private <T> void restoreAdditionalData(final FunctionalBlockBackupHandler<T, ?> handler, final Map<UUID, Object> data,
			final Map<Long, EntityId> restoredAnnotationIdMap) {
		final Map<UUID, T> dataConverted = new HashMap<>(data.size());
		for (final Map.Entry<UUID, Object> fbEntry : data.entrySet()) {
			final FunctionalBlockAdditionalData additionalData = objectMapper.convertValue(fbEntry.getValue(), FunctionalBlockAdditionalData.class);
			dataConverted.put(fbEntry.getKey(), objectMapper.convertValue(additionalData.getData(), handler.getInputType()));
		}
		final FunctionalBlockBackupContext context = new FunctionalBlockBackupContext(restoredAnnotationIdMap);
		handler.restore(context, dataConverted);
	}
}
