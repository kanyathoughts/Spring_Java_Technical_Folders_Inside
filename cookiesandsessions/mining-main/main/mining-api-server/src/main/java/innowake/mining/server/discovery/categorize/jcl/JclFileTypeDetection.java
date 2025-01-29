/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.jcl;

import static innowake.mining.shared.model.discovery.ResolveTarget.JCL_JOB;
import static innowake.mining.shared.model.discovery.ResolveTarget.JCL_PROC;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.jcl.ast.JclNodeTypeCategory;
import innowake.ndt.jcl.parser.model.JCL;
import innowake.ndt.jcl.parser.model.StepExec;
import innowake.ndt.jcl.parser.model.StepIf;
import innowake.ndt.jcl.parser.parser.JCLStatements;

/**
 * File type detection for JCL.
 */
public class JclFileTypeDetection extends AbstractFileTypeDetection {
	
	public static final String PROCS_CACHE_KEY = ResolveTarget.JCL.name() + "Procs";
	public static final String INCLUDES_CACHE_KEY = ResolveTarget.JCL.name() + "Includes";
	public static final String CONTROL_CARDS_CACHE_KEY = ResolveTarget.JCL.name() + "CCs";
	
	private static final Pattern JOB_CARD_PATTERN = Pattern.compile("^ *//[^\\*\\s]{1,8} *JOB");
	
	private static final String PGM = "PGM";
	
	private final JclParseResultProvider parseResultProvider;
	private final Config config;
	private final DiscoveryJobCache discoveryCache;
	private final String jobId;

	public JclFileTypeDetection(final Config config, final JclParseResultProvider parseResultProvider, final DiscoveryJobCache discoveryCache, final String jobId) {
		super(getLanguage());
		this.config = config;
		this.parseResultProvider = parseResultProvider;
		this.discoveryCache = discoveryCache;
		this.jobId = jobId;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final IAst<SourcePojo> ast;
		try {
			ast = parseResultProvider.getLightweight(resource);
		} catch (final Exception e) {
			LOG.error(() -> String.format("Unable to parse %s file as JCL with lightweight parser.", resource), e);
			/* ignore error as we want to try with other files */
			return null;
		}

		/* JWA - this matching of node keywords is just a little too simple (see WCFD-693) */
		boolean isJob = ArrayUtils.isNotEmpty(JclNodeTypeCategory.JOB.getNodes(ast));
		boolean isProc = ArrayUtils.isNotEmpty(JclNodeTypeCategory.PROC.getNodes(ast));

		/* JWA - can't be job and a proc, must resolve (see WCFD-693) */
		if (isJob && isProc) {
			final Matcher matcher = JOB_CARD_PATTERN.matcher(resource.getContent().toString()); 
			if (matcher.find()) {
				/* job statement is present, this is a job */
				isJob = true;
				isProc = false;
			} else {
				/* job statement not present, this is a proc */
				isJob = false;
				isProc = true;
			}
		}
		
		final Identification result;
		if (isJob) {
			result = new Identification(ID.YES, resource.getId(), JCL_JOB, getLanguage());
		} else if (isProc) {
			result = new Identification(ID.YES, resource.getId(), JCL_PROC, getLanguage());
		} else {
			result = null;
		}
		
		if (result != null && result.getId() != ID.NO) {
			LOG.debug(() -> "Filetype detection JCL: " + result);
		}
		return result;
	}
	
	@Override
	public boolean identifyDependencies(final SourcePojo sourceObject) {
		boolean identifiedAnyDependencies = false;
		try {
			final boolean isParsable = JobControl.isParsable(sourceObject);
			if (isParsable || sourceObject.getTechnology() == Technology.JCL && sourceObject.getType() == Type.PROC) {
				final String content = sourceObject.getContent().toString();
				if (isParsable) {
					final List<JCL> jclParseResult = parseResultProvider.getParseResult(sourceObject).getFull();
					final Set<String> procCalls = findPotentialProcCalls(jclParseResult);
					identifiedAnyDependencies = ! procCalls.isEmpty();
					procCalls.parallelStream()
						.forEach(proc -> {
						discoveryCache.putMultiValue(jobId, PROCS_CACHE_KEY, proc);
					});

					final Set<String> includeMembers = JobControl.findIncludeMembers(content);
					identifiedAnyDependencies |= ! includeMembers.isEmpty();
					includeMembers.parallelStream()
							.forEach(include -> discoveryCache.putMultiValue(jobId, INCLUDES_CACHE_KEY, include));
					
					final Set<String> ccMembers = findAllControlCards(jclParseResult);
					identifiedAnyDependencies |= ! ccMembers.isEmpty();
					ccMembers.parallelStream()
							.forEach(cc -> discoveryCache.putMultiValue(jobId, CONTROL_CARDS_CACHE_KEY, cc));
				}
			}
		} catch (final WorkerCancellationException e) {
			/* explicitly ignore the exception */
			LOG.debug(() -> String.format("[%s] JCL: parsing cancelled due to timeout", sourceObject.getName()));
		} catch (final Exception e) {
			LOG.error(() -> String.format("Unable to parse %s file as JCL.", sourceObject.getPath()), e);
			/* ignore error as we want to try with other files */
		}
		return identifiedAnyDependencies;
	}
	
	private Set<String> findPotentialProcCalls(final List<JCL> jclParseResult) {		
			return jclParseResult.stream()
					.flatMap(jcl -> jcl.getAllSteps().parallelStream())
					.filter( step -> step.getProperties().get(JCLStatements.PROC) != null)
					.map( step -> step.getProperties().get(JCLStatements.PROC))
					.collect(Collectors.toSet());
	}

	private Set<String> findAllControlCards(final List<JCL> jclParseResult) {
		return jclParseResult.stream()
				.flatMap(JobControl::getJCLStepsRecursively)
				.filter(steps -> steps instanceof StepExec || steps instanceof StepIf)
				.filter(step -> step.getProperties().get(PGM) != null)
				.flatMap(step -> config.getPgmStepNameList().stream().map(step.getDDs()::get))
				.filter(Objects::nonNull)
				.flatMap(List::stream)
				.map(JobControl::getControlCardName)
				.filter(StringUtils::isNotEmpty)
				.collect(Collectors.toSet());
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.JCL;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

}
