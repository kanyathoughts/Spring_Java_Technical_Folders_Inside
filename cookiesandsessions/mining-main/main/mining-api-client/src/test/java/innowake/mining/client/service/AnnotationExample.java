/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.annotation.AnnotationServiceProvider;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationType;

/**
 * Command line runner to test annotations services.
 */
public class AnnotationExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "c8960c7a-6df5-4f60-8f18-249268d9a581");
		final AnnotationServiceProvider service = MiningApiClient.annotationService(connectionInfo);
		
		LOG.info("create new annotation" + MARK);
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("A new annotation 1");
		newAnnotation.setState(WorkingState.CANDIDATE);
		newAnnotation.setType(AnnotationType.RULE);
		LOG.info("Status message: " + service.createAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotation(newAnnotation)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation with empty name" + MARK);
		final AnnotationPojoPrototype newAnnotation2 = new AnnotationPojoPrototype();
		newAnnotation2.setState(WorkingState.CANDIDATE);
		newAnnotation2.setType(AnnotationType.RULE);
		LOG.info("Status message: " + service.createAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotation(newAnnotation2)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation with non existent project" + MARK);
		final AnnotationPojoPrototype newAnnotation3 = new AnnotationPojoPrototype();
		newAnnotation3.setName("A new annotation 3");
		newAnnotation3.setState(WorkingState.CANDIDATE);
		newAnnotation3.setType(AnnotationType.RULE);
		LOG.info("Status message: " + service.createAnnotation()
				.setProjectId(EntityId.of(-1l))
				.setAnnotation(newAnnotation3)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation with different project id in path and DAO" + MARK);
		final AnnotationPojoPrototype newAnnotation4 = new AnnotationPojoPrototype();
		newAnnotation4.setName("A new annotation 4");
		newAnnotation4.setState(WorkingState.CANDIDATE);
		newAnnotation4.setType(AnnotationType.RULE);
		LOG.info("Status message: " + service.createAnnotation()
				.setProjectId(EntityId.of(2l))
				.setAnnotation(newAnnotation4)
				.execute()
				.getStatusMessage());
		
		LOG.info("create new annotation with source attachment" + MARK);
		final AnnotationPojoPrototype newAnnotation5 = new AnnotationPojoPrototype();
		newAnnotation5.setName("A new annotation 5");
		newAnnotation5.setState(WorkingState.CANDIDATE);
		newAnnotation5.setType(AnnotationType.RULE);
		newAnnotation5.setSourceAttachment(new BinaryString("abc def"));
		LOG.info("Status message: " + service.createAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotation(newAnnotation5)
				.execute()
				.getStatusMessage());
		
		LOG.info("find all annotations" + MARK);
		LOG.info("Status message: " + service.findAllAnnotations().setProjectId(EntityId.of(1l)).execute().getStatusMessage());
		
		LOG.info("find all annotations with non existent project" + MARK);
		LOG.info("Status message: " + service.findAllAnnotations().setProjectId(EntityId.of(-1l)).execute().getStatusMessage());
		
		LOG.info("find annotations with id=1" + MARK);
		final Result<AnnotationPojo> foundAnnotationResult = service.findAnnotationById()
				.setProjectId(EntityId.of(1l))
				.setAnnotationId(EntityId.of(1L))
				.execute();
		final AnnotationPojo foundAnnotation = foundAnnotationResult.getValue().orElse(null);
		LOG.info("Status message: " + foundAnnotationResult.getStatusMessage());
		
		LOG.info("find non existent annotation" + MARK);
		LOG.info("Status message: " + service.findAnnotationById()
				.setProjectId(EntityId.of(1l))
				.setAnnotationId(EntityId.of(-1L))
				.execute()
				.getStatusMessage());
		
		LOG.info("update an existing annotation with a new source attachment" + MARK);
		LOG.info("Status message: " + service.updateAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotation(new AnnotationPojoPrototype().withId(foundAnnotation.identity())
				.setSourceAttachment(new BinaryString("A new source for the annotation")))
				.execute()
				.getStatusMessage());
		
		LOG.info("update an existing annotation with an empty name" + MARK);
		LOG.info("Status message: " + service.updateAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotation(new AnnotationPojoPrototype().setNid(1l).setName(""))
				.execute()
				.getStatusMessage());
		
		LOG.info("update a non existing annotation" + MARK);
		LOG.info("Status message: " + service.updateAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotation(new AnnotationPojoPrototype().setNid(99l))
				.execute()
				.getStatusMessage());
		
		LOG.info("update annotation with different project id in path and DAO" + MARK);
		LOG.info("Status message: " + service.updateAnnotation()
				.setProjectId(EntityId.of(2l))
				.setAnnotation(new AnnotationPojoPrototype().setNid(1l))
				.execute()
				.getStatusMessage());
		
		LOG.info("delete annotation" + MARK);
		LOG.info("Status message: " + service.deleteAnnotation()
				.setProjectId(EntityId.of(1l))
				.setAnnotationId(EntityId.of(1L))
				.execute()
				.getStatusMessage());
	}
}
