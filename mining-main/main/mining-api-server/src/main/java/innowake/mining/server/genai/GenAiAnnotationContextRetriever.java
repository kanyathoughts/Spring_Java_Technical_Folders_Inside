/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai;

import java.util.UUID;

/**
 * Interface for retrieving additional GenAI prompt context information for a given annotation.
 * <p>
 * The context retrievers are called in a chain, so that each retriever can add additional context information.
 * If a retriever does not have any context information to add, it should return the original object.
 * If the context should be cleared an empty context via {@link AiGenerationContext#EMPTY} can be returned.
 */
public interface GenAiAnnotationContextRetriever {

	/**
	 * Retrieve prompt context information for a given annotation.
	 *
	 * @param context the annotation context
	 * @param requestUUID the uuid associated with the annotation description generation
	 * @return the context information, or {@link AiGenerationContext#EMPTY} if the context should be cleared
	 */
	AiGenerationContext retrieve(AnnotationContext context, UUID requestUUID);
}
