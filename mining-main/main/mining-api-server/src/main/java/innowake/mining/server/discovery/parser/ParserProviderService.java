/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import java.util.Map;

import innowake.mining.server.discovery.parser.sqllightweight.SqlLightWeightParseResultProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.metrics.SourceExportService;
import innowake.mining.server.discovery.metrics.ims.ImsDataProvider;
import innowake.mining.server.discovery.metrics.vms.IFDLLightParser;
import innowake.mining.server.discovery.parser.assembler.AssemblerParserResultProvider;
import innowake.mining.server.discovery.parser.basic.BasicParseResultProvider;
import innowake.mining.server.discovery.parser.batch.DiscoveryJclContentProvider;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.discovery.parser.c.CAntlrParseResultProvider;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.server.discovery.parser.csd.CsdParseResultProvider;
import innowake.mining.server.discovery.parser.dcl.DCLParseResultProvider;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResultProvider;
import innowake.mining.server.discovery.parser.ecl.EclParseResultProvider;
import innowake.mining.server.discovery.parser.ims.ImsParseResultProvider;
import innowake.mining.server.discovery.parser.java.JavaParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.discovery.parser.oracle.CDORecordParser;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.discovery.parser.plsql.PlSqlParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.jcl.parser.assembling.IContentProvider;
import innowake.ndt.mfsparser.parser.MfsParserAst;
import innowake.ndt.parsing.parser.csd.CsdParserFactory.CsdParserType;

/**
 * A service that creates new instances of parsers and {@link ParseResultProvider ParseResultProviders}.
 */
@Service
public class ParserProviderService {

	private final SourceCachingService sourceService;
	private final DiscoveryCache discoveryCache;
	private final SourceExportService sourceExportService;
	private final ParseResultCacheService parseResultCacheService;

	@Autowired
	public ParserProviderService(final SourceCachingService sourceService, final DiscoveryCache discoveryCache, final SourceExportService sourceExportService, 
			final ParseResultCacheService parseResultCacheService) {
		this.sourceService = sourceService;
		this.discoveryCache = discoveryCache;
		this.sourceExportService = sourceExportService;
		this.parseResultCacheService = parseResultCacheService;
	}

	/**
	 * @return a new instance of the {@link CDORecordParser}.
	 */
	public CDORecordParser createCdoRecordParser() {
		return new CDORecordParser();
	}

	/**
	 * Returns a new instance of the {@link AssemblerParserResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link AssemblerParserResultProvider} instance
	 */
	public AssemblerParserResultProvider createAssemblerParser(final DiscoveryContext context) {
		return createAssemblerParser(context.getTimedWorker(), context.getSearchOrders(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link AssemblerParserResultProvider}.
	 *
	 * @param timedWorker the worker for executing the parser tasks
	 * @param searchOrders the search order of the current project
	 * @param jobId the id of the current job
	 * @return new {@link AssemblerParserResultProvider} instance
	 */
	public AssemblerParserResultProvider createAssemblerParser(final TimedWorker timedWorker, final SearchOrders searchOrders, final String jobId) {
		final SourceObjectResolver assemblerSourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		return new AssemblerParserResultProvider(timedWorker, assemblerSourceObjectResolver, jobId, parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link BasicParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link BasicParseResultProvider} instance
	 */
	public BasicParseResultProvider createBasicParser(final DiscoveryContext context) {
		return createBasicParser(context.getConfig(), context.getTimedWorker(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link BasicParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @return new {@link BasicParseResultProvider} instance
	 */
	public BasicParseResultProvider createBasicParser(final Config config, final TimedWorker timedWorker, final String jobId) {
		return new BasicParseResultProvider(config, timedWorker, jobId, parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link CAntlrParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link CAntlrParseResultProvider} instance
	 */
	public CAntlrParseResultProvider createCParser(final DiscoveryContext context) {
		return createCParser(context.getConfig(), context.getTimedWorker(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link CAntlrParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @return new {@link CAntlrParseResultProvider} instance
	 */
	public CAntlrParseResultProvider createCParser(final Config config, final TimedWorker timedWorker, final String jobId) {
		return new CAntlrParseResultProvider(config, timedWorker, jobId, parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link CobolParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link CobolParseResultProvider} instance
	 */
	public CobolParseResultProvider createCobolParser(final DiscoveryContext context) {
		return createCobolParser(context.getConfig(), context.getTimedWorker(), context.getSearchOrders(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link CobolParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param searchOrders the search order of the current project
	 * @param jobId the id of the current job
	 * @return new {@link CobolParseResultProvider} instance
	 */
	public CobolParseResultProvider createCobolParser(final Config config, final TimedWorker timedWorker, final SearchOrders searchOrders, final String jobId) {
		final SourceObjectResolver objResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		return new CobolParseResultProvider(config, timedWorker, objResolver, jobId, parseResultCacheService, sourceService, searchOrders, discoveryCache);
	}

	/**
	 * Returns a new instance of the {@link CsdParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @param csdParserType the concrete {@link CsdParserType} of the {@link CsdParseResultProvider}
	 * @return new {@link CsdParseResultProvider} instance
	 */
	public CsdParseResultProvider createCsdParser(final DiscoveryContext context, final CsdParserType csdParserType) {
		return createCsdParser(context.getConfig(), context.getTimedWorker(), context.getJobId(), csdParserType);
	}

	/**
	 * Returns a new instance of the {@link CsdParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @param csdParserType the concrete {@link CsdParserType} of the {@link CsdParseResultProvider}
	 * @return new {@link CsdParseResultProvider} instance
	 */
	public CsdParseResultProvider createCsdParser(final Config config, final TimedWorker timedWorker, final String jobId, final CsdParserType csdParserType) {
		return new CsdParseResultProvider(config, csdParserType, timedWorker, jobId, parseResultCacheService);
	}
	
	/**
	 * Returns a new instance of the {@link DCLParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link DCLParseResultProvider} instance
	 */
	public DCLParseResultProvider createDclParser(final DiscoveryContext context) {
		return createDclParser(context.getConfig(), context.getTimedWorker(), context.getSearchOrders(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link DCLParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param searchOrders the search order of the current project
	 * @param jobId the id of the current job
	 * @return new {@link DCLParseResultProvider} instance
	 */
	public DCLParseResultProvider createDclParser(final Config config, final TimedWorker timedWorker, final SearchOrders searchOrders, final String jobId) {
		final SourceObjectResolver dclSourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		return new DCLParseResultProvider(config, timedWorker, jobId, parseResultCacheService, dclSourceObjectResolver);
	}
	
	/**
	 * Returns a new instance of the {@link EasytrieveAntlrParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link EasytrieveAntlrParseResultProvider} instance
	 */
	public EasytrieveAntlrParseResultProvider createEasytrieveParser(final DiscoveryContext context) {
		return createEasytrieveParser(context.getConfig(), context.getTimedWorker());
	}

	/**
	 * Returns a new instance of the {@link EasytrieveAntlrParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @return new {@link EasytrieveAntlrParseResultProvider} instance
	 */
	public EasytrieveAntlrParseResultProvider createEasytrieveParser(final Config config, final TimedWorker timedWorker) {
		return new EasytrieveAntlrParseResultProvider(config, timedWorker);
	}

	/**
	 * Returns a new instance of the {@link EclParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link EclParseResultProvider} instance
	 */
	public EclParseResultProvider createEclParser(final DiscoveryContext context) {
		return createEclParser(context.getConfig(), context.getTimedWorker(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link EclParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
 	 * @param jobId the id of the current job
	 * @return new {@link EclParseResultProvider} instance
	 */
	public EclParseResultProvider createEclParser(final Config config, final TimedWorker timedWorker, final String jobId) {
		return new EclParseResultProvider(config, timedWorker, jobId, parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link ImsParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link ImsParseResultProvider} instance
	 */
	public ImsParseResultProvider createImsParser(final DiscoveryContext context) {
		return createImsParser(context.getConfig(), context.getTimedWorker(), context.getJobId());
	}
	
	/**
	 * Returns a new instance of the {@link ImsParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
 	 * @param jobId the id of the current job
	 * @return new {@link ImsParseResultProvider} instance
	 */
	public ImsParseResultProvider createImsParser(final Config config, final TimedWorker timedWorker, final String jobId) {
		return new ImsParseResultProvider(config, timedWorker, jobId, parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link JavaParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link JavaParseResultProvider} instance
	 */
	public JavaParseResultProvider createJavaParser(final DiscoveryContext context) {
		return createJavaParser(context.getConfig(), context.getTimedWorker(), context.getSearchOrders(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link JavaParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param searchOrders the search order of the current project
	 * @param jobId the id of the current job
	 * @return new {@link JavaParseResultProvider} instance
	 */
	public JavaParseResultProvider createJavaParser(final Config config, final TimedWorker timedWorker, final SearchOrders searchOrders, final String jobId) {
		return new JavaParseResultProvider(config, timedWorker, sourceExportService.getSourceExportPath(jobId), jobId, parseResultCacheService, searchOrders);
	}

	/**
	 * Returns a new instance of the {@link JclParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link JclParseResultProvider} instance
	 */
	public JclParseResultProvider createJclParser(final DiscoveryContext context) {
		return createJclParser(context.getConfig(), context.getTimedWorker(), context.getJobId(), context.getProjectId());
	}

	/**
	 * Returns a new instance of the {@link JclParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @param projectId the id of the current project
	 * @return new {@link JclParseResultProvider} instance
	 */
	public JclParseResultProvider createJclParser(final Config config, final TimedWorker timedWorker, final String jobId, final EntityId projectId) {
		final IContentProvider jclContentprovider = new DiscoveryJclContentProvider(projectId, sourceService);
		return new JclParseResultProvider(config, timedWorker, jclContentprovider, jobId, parseResultCacheService);
	}
	
	/**
	 * Returns a new instance of the {@link JclParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @param jclContentprovider {@link IContentProvider}
	 * @return new {@link JclParseResultProvider} instance
	 */
	public JclParseResultProvider createJclParser(final DiscoveryContext context, final IContentProvider jclContentprovider) {
		return new JclParseResultProvider(context.getConfig(), context.getTimedWorker(), jclContentprovider, context.getJobId(), parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link NaturalParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link NaturalParseResultProvider} instance
	 */
	public NaturalParseResultProvider createNaturalParser(final DiscoveryContext context) {
		return createNaturalParser(context.getConfig(), context.getTimedWorker(), context.getSearchOrders(), context.getJobId());
	}
	
	/**
	 * Returns a new instance of the {@link NaturalParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @param searchOrders the search order of the current project
	 * @return new {@link NaturalParseResultProvider} instance
	 */
	public NaturalParseResultProvider createNaturalParser(final Config config, final TimedWorker timedWorker, final SearchOrders searchOrders, final String jobId) {
		final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		return new NaturalParseResultProvider(sourceObjectResolver, config, timedWorker, jobId, parseResultCacheService); 
	}

	/**
	 * Returns a new instance of the {@link Pl1ParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link Pl1ParseResultProvider} instance
	 */
	public Pl1ParseResultProvider createPl1Parser(final DiscoveryContext context) {
		return createPl1Parser(context.getConfig(), context.getTimedWorker(), context.getSearchOrders(), context.getJobId(), context.getFeatureMap());
	}

	/**
	 * Returns a new instance of the {@link Pl1ParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @param searchOrders the search order of the current project
	 * @param featureMap  map with the current FF4j feature settings
	 * @return new {@link Pl1ParseResultProvider} instance
	 */
	public Pl1ParseResultProvider createPl1Parser(final Config config, final TimedWorker timedWorker, final SearchOrders searchOrders, final String jobId,
			final Map<FeatureId, Boolean> featureMap) {
		final SourceObjectResolver objectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		return new Pl1ParseResultProvider(config, timedWorker, jobId, parseResultCacheService, objectResolver, featureMap);
	}

	/**
	 * Returns a new instance of the {@link PlSqlParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link PlSqlParseResultProvider} instance
	 */
	public PlSqlParseResultProvider createPlSqlParser(final DiscoveryContext context) {
		return createPlSqlParser(context.getConfig(), context.getTimedWorker(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link PlSqlParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @return new {@link PlSqlParseResultProvider} instance
	 */
	public PlSqlParseResultProvider createPlSqlParser(final Config config, final TimedWorker timedWorker, final String jobId) {
		return new PlSqlParseResultProvider(config, timedWorker, jobId, parseResultCacheService);
	}

	/**
	 * Returns a new instance of the {@link SqlLightWeightParseResultProvider}.
	 *
	 * @param context  the current discovery context
	 * @return new {@link SqlLightWeightParseResultProvider} instance
	 */
	public SqlLightWeightParseResultProvider createSqlLightWeightParser(final DiscoveryContext context) {
		return createSqlLightWeightParser(context.getConfig(), context.getTimedWorker(), context.getJobId());
	}

	/**
	 * Returns a new instance of the {@link SqlLightWeightParseResultProvider}.
	 *
	 * @param config the discovery configuration
	 * @param timedWorker the worker for executing the parser tasks
	 * @param jobId the id of the current job
	 * @return new {@link SqlLightWeightParseResultProvider} instance
	 */
	public SqlLightWeightParseResultProvider createSqlLightWeightParser(final Config config, final TimedWorker timedWorker, final String jobId) {
		return new SqlLightWeightParseResultProvider(config, timedWorker, jobId, parseResultCacheService);
	}
	
	/**
	 * Returns a new instance of the {@link MfsParserAst}.
	 * 
	 * @return new {@link MfsParserAst} instance
	 */
	public MfsParserAst<SourcePojo> createImsMfsParser() {
		return new MfsParserAst<>(
				new BaseParserConfiguration.Builder<SourcePojo>().setAssemblingDataProvider(new ImsDataProvider()).build());
	}
	
	/**
	 * Returns a new instance of the {@link IFDLLightParser}.
	 * 
	 * @return new {@link IFDLLightParser} instance
	 */
	public IFDLLightParser createIfdlLightParser() {
		return new IFDLLightParser();
	}
}
