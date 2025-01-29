/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.csd;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.parsing.parser.csd.CsdParser;
import innowake.ndt.parsing.parser.csd.CsdParserFactory;
import innowake.ndt.parsing.parser.csd.CsdParserFactory.CsdParserType;
import innowake.ndt.parsing.parser.csd.model.Csd;

/**
 * Provide CSD Parser results.
 * It also caches the {@link Csd} parse result.
 */
public class CsdParseResultProvider extends AbstractCachingParseResultProvider<Csd> {

	private final CsdParser csdParser;
	private final Map<String, String> parseErrors = new HashMap<>();
	private final CsdParserType type;

	/**
	 * Creates an instance of CsdParserResultProvider.
	 * 
	 * @param config the configuration
	 * @param type the CSD parser type
	 * @param worker executor for the parsing
	 * @param jobId the job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public CsdParseResultProvider(final Config config, final CsdParserType type,
			final TimedWorker worker, final String jobId, final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.CSD), jobId, parseResultCacheService);
		this.type = type;
		this.csdParser = CsdParserFactory.createParser(type);
	}
	
	@Override
	public Csd getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject, 
						MessageProvider.from(sourceObject, ResolveTarget.CSD),
						() -> csdParser.parse(sourceObject.getContent().toString()));
		} catch (final WorkerException exception) {
			parseErrors.put(sourceObject.getPath(), "[" + sourceObject.getName() + "] " +
					"CSD Parser gets an error." + exception.getMessage());
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}

	/**
	 * Returns the parse error.
	 *
	 * @param file the source object
	 * @return the parse error
	 */
	public Optional<String> getParseError(final SourcePojo file) {
		return Optional.ofNullable(parseErrors.get(file.getPath()));
	}

	/**
	 * Returns the key for the given {@code sourceObject}.
	 * <p>The implementation additionally adds the {@link CsdParserType} to the path and job id since the parse results will be type dependent.</p>
	 * <pre>jobId + "@" + sourceObject.getPath() + "$" + type</pre>
	 *
	 * @param sourceObject The {@link SourcePojo} for calculating the key for caching
	 * @return the cache key
	 */
	@Override
	protected String getKey(final SourcePojo sourceObject) {
		return super.getKey(sourceObject) + "$" + type;
	}
}
