/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.extensions.metadata.model.ModuleBackup;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * MetaDataExportModuleTask creates the {@linkplain ModuleBackup back up module} with it's associated meta data.
 */
public class MetaDataExportModuleTask extends Task<ModuleBackup> {

	@Autowired
	private transient AnnotationService annotationService;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient DataDictionaryService dataDictionaryService;
	@Autowired
	private transient TaxonomyService taxonomyService;

	private final EntityId projectId;
	private final EntityId moduleId;

	public MetaDataExportModuleTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId);
		this.projectId = projectId;
		this.moduleId = moduleId;
	}

	@Override
	protected Result<ModuleBackup> run(final ProgressMonitor progressMonitor) {
		final ModuleBackup moduleBackup = new ModuleBackup();
		final ModulePojo module = moduleService.getModule(moduleId);

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> dataDictionaryEntries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleId));

		moduleBackup.setName(module.getName());
		moduleBackup.setPath(module.getPath().orElse(null));
		moduleBackup.setTechnology(module.getTechnology());
		moduleBackup.setType(module.getType());
		moduleBackup.setContentHash(module.getContentHash().map(BinaryValue::get).orElse(null));
		moduleBackup.setCustomProperties(module.getCustomProperties());
		moduleBackup.setDescription(module.getDescription().orElse(null));
		moduleBackup.setAnnotations(annotations);
		moduleBackup.setDataDictionaryEntries(dataDictionaryEntries);
		moduleBackup.setTaxonomies(taxonomies);
		moduleBackup.setRequiresReview(module.isRequiresReview());
		moduleBackup.setLinkHash(module.getLinkHash());

		return new Result<>(moduleBackup);
	}

}
