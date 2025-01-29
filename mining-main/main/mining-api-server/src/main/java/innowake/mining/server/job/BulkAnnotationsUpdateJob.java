/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.lang.DiffSet;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationUpdateType;


/**
 * Job for List of {@link AnnotationPojo AnnotationPojo's} categories and metadata update.
 */
public class BulkAnnotationsUpdateJob extends MiningJob<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(BulkAnnotationsUpdateJob.class);
	
	private static final int BATCH_SIZE = 1000;
	
	private final List<AnnotationPojoPrototype> annotations;
	
	@Autowired
	private transient AnnotationService annotationService;
	
	/**
	 * Creates a new BulkAnnotationsUpdateJob instance.
	 * @param annotationService AnnotationService instance
	 * @param projectId the project ID of the {@linkplain AnnotationPojo AnnotationPojo's} to update.
	 * @param annotations the list of {@linkplain AnnotationPojo} to be updated.
	 * @param updateType the type of the annotation update
	 */
	public BulkAnnotationsUpdateJob(final AnnotationService annotationService, final EntityId projectId, final List<AnnotationPojo> annotations, final String updateType) {
		super(projectId);
		this.annotationService = annotationService;
		
		final var update = Stream.<AnnotationPojoPrototype>builder();
		
		if (updateType.equals(AnnotationUpdateType.BUSINESS_RELATED_FROM_NO_TO_YES.getType())) {
			final AnnotationCategory category = annotationService.findCategory(q -> q.ofProjectWithDefault(projectId).withName("Business Rule"))
					.orElseThrow(() -> new MiningEntityNotFoundException("Annotation category 'Business Rule' must exists"));
			for (final AnnotationPojo annotation : annotations) {
				update.accept(new AnnotationPojoPrototype().withId(annotation.identity())
					.setCategoryId(category.getId())
					.setReasons(new DiffSet<String>().addition(AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED.name())));
			}
		} else if (updateType.equals(AnnotationUpdateType.BUSINESS_RELATED_FROM_YES_TO_NO.getType())) {
			for (final AnnotationPojo annotation : annotations) {
				if (AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName().equals(annotation.getCategoryName().orElse(""))) {
					throw new IllegalStateException("All the annotation categories must be updated to non business categories");
				}
				update.accept(new AnnotationPojoPrototype().withId(annotation.identity())
					.setCategoryId(annotation.getCategoryId().orElseThrow(() -> new IllegalStateException("Annotation must have a category: " + annotation.toString())))
					.setReasons(new DiffSet<String>().deletion(AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED.name())));
			}
		} else {
			throw new IllegalArgumentException(updateType);
		}
		
		this.annotations = update.build().collect(Collectors.toList());
	}
	
	@Override
	public String getJobName() {
		return "Bulk Annotations Update";
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		LOG.debug("Starting Bulk Annotations Update Job");
		progressMonitor.setJobDescription("Bulk Annotations Update");
		return updateAnnotations(progressMonitor);
	}

	private Result<Serializable> updateAnnotations(final ProgressMonitor progressMonitor) {
		try {
			progressMonitor.setStepDescription("Starting Bulk Annotations Update Job");
			annotationService.update(annotations, BATCH_SIZE);
			progressMonitor.setStepDescription("Bulk Annotations Update Finished");
		} catch (final Exception e) {
			return new Result<>(new Status(e));
		}
		
		return new Result<>(Status.OK);
	}

}
