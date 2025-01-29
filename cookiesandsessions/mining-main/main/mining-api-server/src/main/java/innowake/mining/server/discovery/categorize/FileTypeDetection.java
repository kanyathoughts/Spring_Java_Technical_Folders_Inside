package innowake.mining.server.discovery.categorize;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Base interface for all file type detections.
 * Implementations are used to identify
 * <li> If the given file is a main type. Example Cobol program
 * <li> If the given file is a dependency of an already identified main type. Example Cobol copybook.
 * <li> If the given file is a type by analyzing the content for keywords.
 * <p>
 * <b>Note:</b> Every implementation requires a public static method {@code getLanguage()} that returns
 * the language this implementation is for and a public static method {@code getDetectionPhases()} that
 * returns the supported phases. Example:
 * <pre>
 * public static ResolveTarget getLanguage() {
 *	return ResolveTarget.COBOL;
 * }
 *	
 * public static Set&lt;DetectionPhase&gt; getDetectionPhases() {
 *	return Sets.newHashSet(DetectionPhase.MAIN);
 * }
 * </pre>
 */
public interface FileTypeDetection {
	
	/**
	 * Supported phases a {@link FileTypeDetection} can be executed with.
	 */
	public enum DetectionPhase {
		/** Any preparational work to pre-collect required information. */
		PREPARE,
		/** The default main detection phase. */
		MAIN;
	}
	
	/**
	 * Identify if the resource is a language main type like a Cobol program.
	 *
	 * @param sourceObject The sourceObject to check.
	 * @return The identification or {@code null}
	 * @throws DiscoveryException Thrown if this call fails.
	 */
	@Nullable
	Identification identifyMainObject(SourcePojo sourceObject) throws DiscoveryException;
	
	/**
	 * Identify if the resource is a language main type with multiple {@linkplain DetectionPhase DetectionPhases}.
	 *
	 * @param sourceObject the source object to check
	 * @param phase the {@link DetectionPhase}
	 * @param monitor the {@link ProgressMonitor}
	 * @return The identification or {@code null}
	 * @throws DiscoveryException Thrown if this call fails.
	 */
	@Nullable
	Identification identifyMainObjectMultiPhase(SourcePojo sourceObject, DetectionPhase phase, ProgressMonitor monitor) throws DiscoveryException;

	/**
	 * Identify all matching dependencies for the given unidentified sourceObject. The results
	 * should be stored in the {@link DiscoveryJobCache} to avoid de-/serialization of possible
	 * large task results.
	 *
	 * @param sourceObject the source object to identify the dependencies for
	 * @return {@code true} if any dependencies had been identified; {@code false} otherwise 
	 */
	default boolean identifyDependencies(SourcePojo sourceObject) {
		/* do nothing by default */
		return false;
	}
	
	/**
	 * Identify a language by content analysis. For example tokenizing.
	 * 
	 * @param resource The file resource to analyze.
	 * @return The identification.
	 */
	@Nullable
	Identification identifyByContent(SourcePojo resource);
	
	/**
	 * Identify a language by file extension.
	 * 
	 * @param resource The file resource to analyze.
	 * @return The identifications for the list of files.
	 */
	@Nullable
	Identification identifyByExtension(SourcePojo resource);

}
