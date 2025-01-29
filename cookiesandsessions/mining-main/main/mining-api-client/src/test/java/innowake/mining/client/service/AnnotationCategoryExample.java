/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import static java.lang.Long.valueOf;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.annotationcategory.AnnotationCategoryServiceProvider;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.shared.model.AnnotationCategory;

/**
 * Command line runner to test annotation categories services.
 */
public class AnnotationCategoryExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "c8960c7a-6df5-4f60-8f18-249268d9a581");
		final AnnotationCategoryServiceProvider service = MiningApiClient.annotationCategoryService(connectionInfo);

		LOG.info("find all annotation categories" + MARK);
		LOG.info("Status message: " + service.findAllAnnotationCategories().setProjectId(valueOf(1)).execute().getStatusMessage());
		
		LOG.info("find all annotation categories with non existent project" + MARK);
		LOG.info("Status message: " + service.findAllAnnotationCategories().setProjectId(valueOf(-1)).execute().getStatusMessage());
		
		LOG.info("find annotation categories with id=1" + MARK);
		final Result<AnnotationCategory> foundAnnotationCategoryResult = service.findAnnotationCategoryById()
				.setProjectId(valueOf(1))
				.setAnnotationCategoryId(valueOf(1))
				.execute();
		final AnnotationCategory foundAnnotationCategory = foundAnnotationCategoryResult.getValue().orElse(null);
		LOG.info("Status message: " + foundAnnotationCategoryResult.getStatusMessage());
		
		LOG.info("find non existent annotation category" + MARK);
		LOG.info("Status message: " + service.findAnnotationCategoryById()
				.setProjectId(valueOf(1))
				.setAnnotationCategoryId(valueOf(-1))
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation category" + MARK);
		final AnnotationCategory newAnnotationCategory = new AnnotationCategory();
		newAnnotationCategory.setName("A new annotation category 3");
		newAnnotationCategory.setProjectId(1L);
		LOG.info("Status message: " + service.createAnnotationCategory()
				.setProjectId(valueOf(1))
				.setAnnotationCategory(newAnnotationCategory)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation category with empty name" + MARK);
		final AnnotationCategory newAnnotationCategory2 = new AnnotationCategory();
		newAnnotationCategory2.setProjectId(1L);
		LOG.info("Status message: " + service.createAnnotationCategory()
				.setProjectId(valueOf(1))
				.setAnnotationCategory(newAnnotationCategory2)
				.execute()
				.getStatusMessage());
		
		LOG.info("update an existing annotation category with a new name" + MARK);
		foundAnnotationCategory.setName("A new name for the annotation category");
		LOG.info("Status message: " + service.updateAnnotationCategory()
				.setProjectId(valueOf(1))
				.setAnnotationCategory(foundAnnotationCategory)
				.execute()
				.getStatusMessage());
		
		LOG.info("update an existing annotation category with an existing name" + MARK);
		foundAnnotationCategory.setName("Annotation Category B");
		LOG.info("Status message: " + service.updateAnnotationCategory()
				.setProjectId(valueOf(1))
				.setAnnotationCategory(foundAnnotationCategory)
				.execute()
				.getStatusMessage());
		
		LOG.info("update an existing annotation category with an empty name" + MARK);
		final AnnotationCategory newAnnotationCategory3 = new AnnotationCategory();
		newAnnotationCategory3.setId(1);
		newAnnotationCategory3.setProjectId(1L);
		LOG.info("Status message: " + service.updateAnnotationCategory()
				.setProjectId(valueOf(1))
				.setAnnotationCategory(newAnnotationCategory3)
				.execute()
				.getStatusMessage());
		
		LOG.info("update a non existing annotation category" + MARK);
		final AnnotationCategory newAnnotationCategory4 = new AnnotationCategory();
		newAnnotationCategory4.setId(99);
		newAnnotationCategory4.setProjectId(1L);
		newAnnotationCategory4.setName("annotation category does not exist");
		LOG.info("Status message: " + service.updateAnnotationCategory()
				.setProjectId(valueOf(1))
				.setAnnotationCategory(newAnnotationCategory4)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation category with non existent project" + MARK);
		final AnnotationCategory newAnnotationCategory5 = new AnnotationCategory();
		newAnnotationCategory5.setId(5);
		newAnnotationCategory5.setName("A new annotation category 5");
		newAnnotationCategory5.setProjectId(-1L);
		LOG.info("Status message: " + service.createAnnotationCategory()
				.setProjectId(valueOf(-1))
				.setAnnotationCategory(newAnnotationCategory5)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation category with different project id in path and DAO" + MARK);
		final AnnotationCategory newAnnotationCategory6 = new AnnotationCategory();
		newAnnotationCategory6.setId(5);
		newAnnotationCategory6.setName("A new annotation category 5");
		newAnnotationCategory6.setProjectId(1L);
		LOG.info("Status message: " + service.createAnnotationCategory()
				.setProjectId(valueOf(2))
				.setAnnotationCategory(newAnnotationCategory6)
				.execute()
				.getStatusMessage());
		
		LOG.info("update annotation category with different project id in path and DAO" + MARK);
		LOG.info("Status message: " + service.createAnnotationCategory()
				.setProjectId(valueOf(2))
				.setAnnotationCategory(foundAnnotationCategory)
				.execute()
				.getStatusMessage());
	}
}
