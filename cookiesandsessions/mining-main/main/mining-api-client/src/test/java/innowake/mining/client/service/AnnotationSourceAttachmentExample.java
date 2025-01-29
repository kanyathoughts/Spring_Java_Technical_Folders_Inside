/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import static java.lang.Long.valueOf;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.annotation.AnnotationServiceProvider;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationType;

/**
 * Command line runner to test annotations services.
 */
public class AnnotationSourceAttachmentExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "");
		final AnnotationServiceProvider service = MiningApiClient.annotationService(connectionInfo);
		
		LOG.info("create new annotation" + MARK);
		
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("A new annotation");
		newAnnotation.setState(WorkingState.CANDIDATE);
		newAnnotation.setType(AnnotationType.RULE);
		newAnnotation.setSourceAttachment("abc");
		final Result<AnnotationPojo> result = service.createAnnotation()
			.setProjectId(valueOf(1))
			.setAnnotation(newAnnotation)
			.execute();
		LOG.info("Status message: " + result.getStatusMessage());
		
		LOG.info("delete annotation" + MARK);
		LOG.info("Status message: " + service.deleteAnnotation()
				.setProjectId(valueOf(1))
				.setAnnotationId(result.getValue().orElseThrow().identity())
				.execute()
				.getStatusMessage());
	}
}
