/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.pl1;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang.exception.ExceptionUtils;
import com.google.common.collect.Sets;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.pl1.Pl1LexerConfiguration.Pl1LexerConfigurationBuilder;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.parsing.base.pattern.RelaxedPatternMatcher;
import innowake.ndt.parsing.parser.base.TokenSequence;
import innowake.ndt.parsing.parser.dependency.ast.BaseDependencyNode;
import innowake.ndt.parsing.scanner.pl1.PL1LexerFactory;

/**
 * File type detection for PL1.
 */
public class Pl1FileTypeDetection extends AbstractFileTypeDetection {
	
	private static final RelaxedPatternMatcher MAIN_PROCEDURE_PATTERN = new Pl1MainProcedurePattern();
	private static final RelaxedPatternMatcher PROCEDURE_PATTERN = new Pl1ProcedurePattern();
	
	private static final RelaxedPatternMatcher[] NON_RELIABLE_PATTERNS = new RelaxedPatternMatcher[] { 
			new Pl1SimpleDeclarePattern(),
			new Pl1DeclarePattern(),
			new Pl1PreprocessorMacroPattern(),
			PROCEDURE_PATTERN
	};
	
	public static final String CACHE_KEY = ResolveTarget.PL1.name();
	private final TokenPartitioner2 tokenPartitioner;
	private final Pl1ParseResultProvider pl1ParserResultProvider;
	private final DiscoveryJobCache discoveryCache;
	private final String jobId;

	public Pl1FileTypeDetection(final Config config, final TimedWorker worker, final DiscoveryJobCache discoveryCache, final String jobId,
			final ParseResultCacheService parseResultCacheService, final Map<FeatureId, Boolean> featureMap, final PersistingSourceObjectResolver sourceObjectResolver) {
		super(getLanguage());
		tokenPartitioner = TokenPartitioner2.create(
				PL1LexerFactory.get(new Pl1LexerConfigurationBuilder().setMargin(config.getPL1ParserMarginStart(), config.getPL1ParserMarginEnd()).build()));
		this.pl1ParserResultProvider = new Pl1ParseResultProvider(config, worker, jobId, parseResultCacheService, sourceObjectResolver, featureMap);
		this.discoveryCache = discoveryCache;
		this.jobId = jobId;
	}
	
	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		Identification result;
		final String content = resource.getContent().toString();
	
		/* try fast analyzing first */
		final ITokenPartitioning tokens = tokenPartitioner.doPartitioning(new StringContent(content));
		result = processReliable(resource, tokens);
		if (result == null) {
			result = processNonReliable(resource, tokens);
		}

		/* if successful try parser */
		if (result != null) {
			LOG.debug(() -> "Filetype detection PL1 parsing object");
			parse(resource);
		}
		
		if (result != null && result.getId() != ID.NO && LOG.isDebugEnabled()) {
			LOG.debug("Filetype detection PL1 result: {}", result);
		}
		return result;
	}
	
	@Override
	public boolean identifyDependencies(final SourcePojo resource) {
		/* All necessary data for this step has already been collected by identifyMainObject.
		 * Therefore for better performance, the logic of this step is done as part of the main job
		 * instead of multiple tasks, since it doesn't involve any additional parsing or any other
		 * heavyweight operation. */
		return false;
	}
	
	/**
	 * Identifies PL1 code as PL1 program type if parsing is successful. 
	 */
	private void parse(final SourcePojo resource) {
		try {
				final Optional<AstNode> root = pl1ParserResultProvider.getParseResult(resource).getPl1IncludeModel().getRoot();
				if (root.isPresent()) {
					root.get().getChildrenDeep(BaseDependencyNode.class).forEach(node -> 
							node.getTargets().forEach(target -> 
									discoveryCache.putMultiValue(jobId, CACHE_KEY, target.toLowerCase())
							)
					);
				}
		} catch (final Exception exception) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Error parsing PL1 for file {}", resource);
			}
			if (LOG.isDebugEnabled()) {
				LOG.debug( ExceptionUtils.getFullStackTrace(exception) );
			}
		}
	}

	@Nullable
	private Identification processReliable(final SourcePojo resource, final ITokenPartitioning tokens) {
		if (MAIN_PROCEDURE_PATTERN.matches(new TokenSequence(tokens, 0))) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.PL1_MAINPROGRAM, getLanguage());
		}
		return null;
	}
	
	@Nullable
	private Identification processNonReliable(final SourcePojo resource, final ITokenPartitioning tokens) {
		final Set<RelaxedPatternMatcher> matches = Arrays.stream(NON_RELIABLE_PATTERNS).filter(pattern -> pattern.matches(new TokenSequence(tokens, 0))).collect(Collectors.toSet());
		final ResolveTarget type = matches.contains(PROCEDURE_PATTERN) ? ResolveTarget.PL1_PROGRAM : ResolveTarget.PL1;
		final int count = matches.size();
		if (count > 1 && count < NON_RELIABLE_PATTERNS.length) {
			return new Identification(ID.MAYBE, resource.getId(), type, getLanguage());
		} else if (count >= NON_RELIABLE_PATTERNS.length) {
			return new Identification(ID.YES, resource.getId(), type, getLanguage());
		} else {
			return null;
		}
	}

	
	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.PL1;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
}
