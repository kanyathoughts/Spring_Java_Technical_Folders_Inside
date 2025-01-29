/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics;

import java.util.List;
import java.util.Optional;

import innowake.mining.shared.access.EntityId;
import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Utility class for metrics
 */
public class MetricsUtility {

	private MetricsUtility() {
		/* no instances allowed */
	}

	/**
	 * Returns string content from the file.
	 * 
	 * @param entry The given {@link ModelArtifact}.
	 * @param projectId the id of the current project
	 * @param sourceService The source object service.
	 * @return String content from the file.
	 */
	@Nullable
	public static String readContent(final EntityId projectId, final ModelArtifact entry, final SourceCachingService sourceService) {
		final Optional<String> path = entry.getPath();
		if ( ! path.isPresent()) {
			return null;
		}

		return sourceService.cachingByProjectPath(projectId.getNid(), path.get()).getContent().toString();
	}
	
	/**
	 * Determine and set the lines of code for an Artifact.
	 * 
	 * @param entry The {@link ModelArtifact} to be processed.
	 * @param projectId The Project Id.
	 * @param commentIdentifier Prefix marking a line as a comment.
	 * @param sourceService The Source Object Dao.
	 * @throws DiscoveryException If something went wrong during collection.
	 */
	public static void collectLinesOfCode(final ModelArtifact entry, final EntityId projectId,
			final String commentIdentifier, final SourceCachingService sourceService) throws DiscoveryException {
		final String path = entry.getPath().orElseThrow(() -> new DiscoveryException("Entry file is not present: " + entry.getName()));
		
		final SourcePojo sourceObject = sourceService.cachingByProjectPath(projectId.getNid(), path);

		final String sourceObjectContent = sourceObject.getContent().toString();
		final String[] content = new Document(sourceObjectContent).lines();
		
		int linesOfComment = 0;
		int linesOfCode = 0;
		for (final String line : content) {
			final String trimmedLine = line.trim();
			if (isComment(trimmedLine, commentIdentifier)) {
				linesOfComment++;
			} else if (!trimmedLine.isEmpty()) {
				linesOfCode++;
			}
		}
		entry.setLinesOfCode(linesOfCode).setLinesOfComments(linesOfComment);
	}

	/**
	 * Adds the content of {@code newElements} to {@code existingElements} if not already contained.
	 * 
	 * @param existingElements the list to add non existing elements of {@code newElements} to
	 * @param newElements the elements to add to {@code existingElements}
	 */
	public static <T extends Object> void mergeList(@Nullable final List<T> existingElements, @Nullable final List<T> newElements) {
		if (existingElements != null && newElements != null) {
			newElements.forEach(e -> {
				if ( ! existingElements.contains(e)) {
					existingElements.add(e);
				}
			});
		}
	}
	
	private static boolean isComment(final String line, final String commentIdentifier) {
		return line.startsWith(commentIdentifier);
	}

	/**
	 * Remove quotes and spaces from both ends of the string.
	 * <pre>
	 * trimQuotesSpaces('abc') = abc
	 * trimQuotesSpace(\abc\) = abc
	 * trimQuotesSpaces(' abc ') = abc
	 * trimQuotesSpaces('abc ') = abc
	 * </pre>
	 * 
	 * @param constant the String to trim quotes and spaces
	 * @return the String with trimmed quotes and spaces from both ends
	 */
	public static String trimQuotesSpaces(final String constant) {
		String result = StringUtils.removeStart(constant, "\"");
		result = StringUtils.removeStart(result, "'");
		result = StringUtils.removeEnd(result, "\"");
		result = StringUtils.removeEnd(result, "'");
		return StringUtils.trim(result);
	}
}
