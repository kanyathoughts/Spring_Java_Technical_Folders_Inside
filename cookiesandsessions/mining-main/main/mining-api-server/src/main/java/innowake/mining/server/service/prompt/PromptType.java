/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service.prompt;

enum PromptType {
	ANNOTATION_GROUP("generateAnnotationGroupDescription.json", "generateAnnotationGroupDescription"),
	REACHABILITY_BLOCK("reachabilityBlockDescription.json", "generateReachabilityBlockDescription"),
	ANNOTATION("generateAnnotationDescription.json", "generateAnnotationDescription"),
	SIMILARITY_SEARCH("generatesSearchSimilarity.json", "similaritySearch");

	private final String fileName;
	private final String useCase;

	PromptType(final String fileName, final String useCase) {
		this.fileName = fileName;
		this.useCase = useCase;
	}

	public String getFileName() {
		return fileName;
	}

	public String getUseCase() {
		return useCase;
	}
}
