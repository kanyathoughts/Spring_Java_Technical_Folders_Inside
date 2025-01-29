/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.ims;

import java.util.Arrays;
import java.util.List;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.parsing.parser.ims.model.dbdgen.DbdGenModel;
import innowake.ndt.parsing.parser.ims.model.psbgen.PsbGenModel;
import innowake.ndt.parsing.parser.ims.parser.DbdParser;
import innowake.ndt.parsing.parser.ims.parser.ImsParser;
import innowake.ndt.parsing.parser.ims.parser.PsbParser;
import innowake.ndt.parsing.parser.ims.parser.Tokenizer;
import innowake.ndt.parsing.parser.ims.sysgen.SysgenParser;
import innowake.ndt.parsing.parser.ims.sysgen.model.SysgenModel;

/**
 * Provides IMS parser results.
 */
public class ImsParseResultProvider extends AbstractCachingParseResultProvider<ImsParseResultProvider.ImsParseResult> {

	/**
	 * Creates an instance of ImsParserResultProvider.
	 * 
	 * @param config The {@link Config}
	 * @param worker The {@link TimedWorker}
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public ImsParseResultProvider(final Config config, final TimedWorker worker, final String jobId,
			final ParseResultCacheService parseResultCacheService) {
		/* Currently there is no IMS config available */
		super(worker, -1, jobId, parseResultCacheService);
	}

	/**
	 * Parse the source object if not present in cache and return parser result otherwise get it from cache. 
	 * 
	 * @param sourceObject to parse
	 * @return the parse result
	 * @throws DiscoveryException if the parsing fails
	 */
	@Override
	public ImsParseResult getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.IMS), () -> new ImsParseResult(sourceObject));
		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException("Exception while getting Ims parser results ", exception);
		}
	}

	private <P extends ImsParser<M>, M> M getModel(final SourcePojo sourceObject, final P parser) throws DiscoveryException {
		try {
			return worker.execute(parser::parse, timeout, UNIT, MessageProvider.from(sourceObject, ResolveTarget.IMS));
		} catch (final WorkerException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}

	/**
	 * An IMS parser result which creates the {@code IMS PSBGEN} and {@code DBD} models when required.
	 */
	public class ImsParseResult {

		private final SourcePojo file;
		@Nullable
		private PsbGenModel psbGenModel;
		@Nullable
		private DbdGenModel dbdGenModel;
		@Nullable
		private SysgenModel sysGenModel;

		private ImsParseResult(final SourcePojo file) {
			this.file = file;
		}

		/**
		 * Creates PsbGenModel for the given file
		 *
		 * @return PsbGenModel IMS ProgramControlBlock Model
		 * @throws DiscoveryException thrown during the failure while creating the PsbGenModel
		 */
		public PsbGenModel createPsbModel() throws DiscoveryException {
			if (psbGenModel != null) {
				return psbGenModel;
			}
			psbGenModel = getModel(file, new PsbParser(new Tokenizer(getLines()), System.err));
			return psbGenModel;
		}

		/**
		 * Creates DbdGenModel for the given file
		 *
		 * @return DbdGenModel DB Model for IMS file
		 * @throws DiscoveryException thrown during the failure while creating the DbdGenModel
		 */
		public DbdGenModel createDbdModel() throws DiscoveryException {
			if (dbdGenModel != null) {
				return dbdGenModel;
			}
			dbdGenModel = getModel(file, new DbdParser(new Tokenizer(getLines()), System.err));
			return dbdGenModel;
		}
		
		public SysgenModel createSysGenModel()throws DiscoveryException {
			if (sysGenModel != null ) {
				return sysGenModel;
			}
			sysGenModel = getModel(file, new SysgenParser(new innowake.ndt.parsing.parser.ims.sysgen.Tokenizer(getLines()), System.err));
			return sysGenModel;
		}
		private List<String> getLines() {
			return Arrays.asList(new Document(file.getContent().toString()).lines());
		}
	}
}
