package innowake.mining.server.discovery.categorize.ims;

import java.util.Set;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * File-type detector for IMS through search by regex patterns.
 */
public class ImsFileTypeDetectionByPattern extends AbstractFileTypeDetection {
	
	private ImsPatternCounter imsPatternCounter = new ImsPatternCounter();
	
	public ImsFileTypeDetectionByPattern() {
		super(getLanguage());
	}
	
	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		final ResolveTarget type = imsPatternCounter.getFileType(content);
		final Identification identification = type != ResolveTarget.NONE ? new Identification(ID.YES, resource.getId(), type, getLanguage()) : null;
		LOG.debug(() -> "Filetype detection " + type.name() + ": " + identification);
    	return identification;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.IMS;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		return null;
	}
}
