package innowake.mining.server.discovery.categorize.ims;

import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_DBD;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_HDAMPARM;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_HELPTXT;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_PSB;

import java.util.Set;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Detector for IMS file-types based on token counting.
 */
public class ImsFileTypeDetection extends AbstractFileTypeDetection {
	
	public ImsFileTypeDetection() {
		super(getLanguage());
	}
	
	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		final ImsTokenCounter tokenCounter = new ImsTokenCounter(content);
	    if (tokenCounter.isPSB()) {
			final Identification identification = new Identification(ID.YES, resource.getId(), IMS_PSB, getLanguage());
			LOG.debug(() -> "Filetype detection IMS_PSB: " + identification);
			return identification;
		}
		else if (tokenCounter.isDBD()){
			final Identification identification = new Identification(ID.YES, resource.getId(), IMS_DBD, getLanguage());
			LOG.debug(() -> "Filetype detection IMS_DBD: " + identification);
			return identification;
		}
		else if (tokenCounter.isDbdh()){
			final Identification identification = new Identification(ID.YES, resource.getId(), IMS_HDAMPARM, getLanguage());
			LOG.debug(() -> "Filetype detection IMS_HDAMPARM: " + identification);
			return identification;
		}
		else if (tokenCounter.isDoc()){
			final Identification identification = new Identification(ID.YES, resource.getId(), IMS_HELPTXT, getLanguage());
			LOG.debug(() -> "Filetype detection IMS_HELPTXT: {}" + identification);
			return identification;
		}	     
	    return null;
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
