/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.easytrieve;

import java.util.HashSet;
import java.util.Set;
import java.util.function.UnaryOperator;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResultProvider;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.antlr.easytrieve.parsers.MacroCall;

public class EasytrieveFileTypeDetection extends AbstractFileTypeDetection {
	
	public static final String CACHE_KEY = ResolveTarget.EASYTRIEVE.name() + "MacroCalls";

	private final SourceService sourceService;
	private final EasytrieveAntlrParseResultProvider antlrResultProvider;
	private final DiscoveryJobCache discoveryCache;
	private final String jobId;

	public EasytrieveFileTypeDetection(final Config config, final TimedWorker worker, final SourceService sourceService,
			final DiscoveryJobCache discoveryCache, final String jobId) {
		super(getLanguage());
		this.sourceService = sourceService;
		antlrResultProvider = new EasytrieveAntlrParseResultProvider(config, worker);
		this.discoveryCache = discoveryCache;
		this.jobId = jobId;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		/* don't do this */
		return null;
	}
	
	@Override
	@Nullable
	public Identification identifyMainObjectMultiPhase(final SourcePojo sourceObject, final DetectionPhase phase, final ProgressMonitor monitor) throws DiscoveryException {
		LOG.trace(() -> "[EZT FileDetection]: categorize by content " + sourceObject.getName());
		if (phase == DetectionPhase.PREPARE) {
			/* prepare phase only checks macros */
			if ( ! sourceObject.getPath().isEmpty()) {
				final Identification identification = antlrResultProvider.checkIfMacroFile(sourceObject);
				/* To minimize the deserialization overhead, we don't store all macros under the same key, but separated per macro name.
				 * This way the task of the main phase doesn't need to always deserialize the whole nested collection with possibly millions
				 * of entries, but only the ones that are actually required. */
				if (identification != null && identification.getId() != ID.NO) {
					discoveryCache.putMultiValue(jobId, getLanguage().name() + sourceObject.getName().trim(), identification.resourceId);
				}
				return identification;
			}
		} else if (phase == DetectionPhase.MAIN) {
			/* In the main phase we try to parse it as an EZT program by utilizing the possible macros identified
			 * in the prepare phase. */
			final Set<MacroCall> macroCalls = new HashSet<>();
			final UnaryOperator<String> getMacroContent = (macroName) -> {
				/* The cache has been populated previously in the prepare phase. */
				final Set<Object> macroResource = discoveryCache.getMultiValue(jobId, getLanguage().name() + macroName.trim());
				if ( ! macroResource.isEmpty()) {
					return sourceService.findContent(EntityId.of((Long) macroResource.iterator().next()))
							.map(BinaryString::toString).orElse(null);
				}
				return null;
			};
			
			final Identification identification = antlrResultProvider.checkIfEztProgram(sourceObject, getMacroContent, macroCalls);
			/* Put all macros into the cache that have now been properly identified by parsing the program. */
			macroCalls.forEach(mc -> discoveryCache.putMultiValue(jobId, CACHE_KEY, mc.macroName));
			if (identification != null && identification.getId() != ID.NO) {
				return identification;
			}
		}
		return null;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.EASYTRIEVE;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.PREPARE, DetectionPhase.MAIN);
	}

}
