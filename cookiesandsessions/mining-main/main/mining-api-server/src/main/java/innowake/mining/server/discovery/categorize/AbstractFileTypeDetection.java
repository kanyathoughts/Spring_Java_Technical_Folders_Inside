package innowake.mining.server.discovery.categorize;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.discovery.Logging;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Abstract base implementation for file type detection.
 */
public abstract class AbstractFileTypeDetection implements FileTypeDetection {

	protected static final Logger LOG = LoggerFactory.getLogger(Logging.CATEGORIZE_FILETYPE_DETECTION);
	
	protected final ResolveTarget language;
	
	/**
	 * Constructor.
	 * 
	 * @param language the language handled by this detection implementation
	 */
	protected AbstractFileTypeDetection(final ResolveTarget language) {
		this.language = language;
	}

	/**
	 * Checks whether the file content contains enough pattern starts
	 * to be classified.
	 * 
	 * @param splitContent lines of the text file.
	 * @param pattern list of matching content.
	 * @param ratioThreshold threshold.
	 * @return true, if the text file matches the pattern, otherwise false.
	 */
	protected boolean discoverByThreshold(final List<String> splitContent, final List<String> pattern, final double ratioThreshold) {
		final long potentialLines = splitContent.stream()
				.filter(line -> pattern.stream().anyMatch(line::startsWith))
				.count();
		return (double) potentialLines / splitContent.size() > ratioThreshold;
	}
	
	/**
	 * Retrieves all the Target types for this file type detection.
	 *
	 * @return Set of {@link ResolveTarget}
	 */
	protected Set<ResolveTarget> getTypes(){
		final Set<ResolveTarget> allTypes = new HashSet<>();
		allTypes.add(language);
		allTypes.addAll(language.getChildren());
		return allTypes;
	}
	
	@Override
	@Nullable
	public Identification identifyByExtension(final SourcePojo sourceObject) {
		final String extension = FilenameUtils.getExtension(sourceObject.getPath());
		if (StringUtils.isBlank(extension)) {
			return null;
		}
		final Optional<ResolveTarget> matchingType = getTypes().stream()
				.filter(type -> ResolveTargetHelper.matchesAnyExtension(type, extension))
				.findFirst();

		if (matchingType.isPresent()) {
			return new Identification(ID.YES, sourceObject.getId(), matchingType.get(), language);
		} else {
			return null;
		}
	}
	
	@Override
	@Nullable
	public Identification identifyMainObjectMultiPhase(final SourcePojo sourceObject, final DetectionPhase phase, final ProgressMonitor monitor) throws DiscoveryException {
		return null;
	}
	
	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo sourceObject) {
		return null;
	}
	
	@Override
	public String toString() {
		return language.name();
	}
	
}
