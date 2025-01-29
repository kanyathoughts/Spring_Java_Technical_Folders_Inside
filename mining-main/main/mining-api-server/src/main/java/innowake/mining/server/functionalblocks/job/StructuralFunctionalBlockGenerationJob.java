/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Job generating functional blocks ({@link FunctionalBlockType#FUNCTIONAL_GROUP} based on a module's structure (e.g. methods, paragraphs).
 */
public class StructuralFunctionalBlockGenerationJob extends ModulesJob {

	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Identified %d Module(s) in selection which are supported by the structural functional block generation.";

	private static final String JOB_DESCRIPTION_PATTERN = "Structural Functional Block Generation for the project %s ";

	@Autowired
	private transient TaxonomyService taxonomyService;

	private final String taxonomyName;

	public StructuralFunctionalBlockGenerationJob(final EntityId projectId, final ModuleMatcher moduleMatcher, final String taxonomyName) {
		super(projectId, moduleMatcher);
		this.taxonomyName = taxonomyName;
	}

	@Override
	protected Task<Serializable> createModuleTask(final ProgressMonitor subMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new StructuralFunctionalBlockGenerationTask(subMonitor, getJobId(), projectId, moduleId);
	}

	@Override
	protected Set<EntityId> filterExecutableModuleIds() {
		final TaxonomyPojo taxonomy;
		if (taxonomyName != null && ! taxonomyName.trim().isEmpty()) {
			taxonomy = taxonomyService.findAny(q -> q.ofProject(projectId).withName(taxonomyName))
					.orElseThrow(() -> new IllegalArgumentException("Taxonomy " + taxonomyName + " does not exist"));
		} else {
			taxonomy = null;
		}

		return new HashSet<>(moduleService.findModuleIds(q -> {
				q.ofProject(projectId).withStorage(Storage.FILE);
				//TODO: disabled in the prototype
				//q.byIds(moduleMatcher.getIds());
				if (taxonomy != null) {
					q.withTaxonomies(List.of(taxonomy.identity()));
				}
			}
		));
	}

	@Override
	protected String getJobDescriptionPattern() {
		return JOB_DESCRIPTION_PATTERN;
	}

	@Override
	protected String getIdentifiedMessagePattern() {
		return IDENTIFIED_MODULES_MESSAGE_PATTERN;
	}

	@Override
	protected List<Tuple2<Technology, Type>> getSupportedModuleTypes() {
		return Collections.emptyList();
	}


	@Override
	public String getJobName() {
		return "Structural Functional Block Generation";
	}
}
