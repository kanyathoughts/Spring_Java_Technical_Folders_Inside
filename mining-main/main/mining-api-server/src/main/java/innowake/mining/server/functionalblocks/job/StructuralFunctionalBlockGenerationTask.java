/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.functionalblocks.generation.StructuralFunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Task generating functional blocks ({@link FunctionalBlockType#FUNCTIONAL_GROUP} based on a module's structure (e.g. methods, paragraphs).
 */
public class StructuralFunctionalBlockGenerationTask extends ModuleTask {

	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;
	@Autowired
	private transient FunctionalBlockComputationService functionalBlockComputationService;

	@Autowired
	private transient ExecutorService executorService;

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the progress monitor to use
	 * @param jobId the Id of the job this task belongs to
	 * @param projectId the Id of the project
	 * @param moduleId the Id of the module
	 */
	public StructuralFunctionalBlockGenerationTask(final ProgressMonitor progressMonitor, final String jobId,
			final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}

	@Override
	protected void run(final EntityId moduleId) {
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(projectId);

		/* I think this is currently required or else we get no sub-blocks for the paragraphs */
		executorService.executeStoreAst(projectId, moduleId);
		/* ensure module block is up-to-date */
		final List<Pair<Operation, UUID>> generated = new ArrayList<>();
		generated.addAll(functionalBlockGenerationService.generate(ModuleBlockGeneration.class, context, moduleId));
		/* do the actual generation */
		generated.addAll(functionalBlockGenerationService.generate(StructuralFunctionalBlockGeneration.class, context, moduleId));

		/* run computation on all generated blocks */
		functionalBlockComputationService.compute(generated.stream()
				.filter(p -> ! p.getLeft().equals(Operation.DELETE))
				.map(Pair::getRight)
				.collect(Collectors.toSet()), getProgressMonitor());
	}
}
