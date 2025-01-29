/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.c;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.exception.ExceptionUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.server.discovery.metrics.c.CHeaderIncludeVisitor;
import innowake.mining.server.discovery.parser.CancellableParserProgressMonitor;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.antlr.base.BaseAntlrErrorListener;
import innowake.ndt.antlr.c.CAntlrParser;
import innowake.ndt.antlr.c.CIncludeOnlyAntlrParser;
import innowake.ndt.antlr.c.CProgramPreProcessor;
import innowake.ndt.core.ParserProgressMonitor;

public class CFileTypeDetection extends AbstractFileTypeDetection {

	public static final String CACHE_KEY = ResolveTarget.C.name();
	/** The {@link TimeUnit} for the parser timeout, currently seconds. */
	private static final TimeUnit UNIT = TimeUnit.SECONDS;
	private static final int FILE_DETECTION_ERROR_LIMIT = 1;

	private final DiscoveryJobCache discoveryCache;
	private final String jobId;
	private final TimedWorker worker;
	private final int timeout;

	public CFileTypeDetection(final Config config, final TimedWorker worker, final DiscoveryJobCache discoveryCache, final String jobId) {
		super(getLanguage());
		this.discoveryCache = discoveryCache;
		this.jobId = jobId;
		this.worker = worker;
		this.timeout = config.getParserTimeout(ResolveTarget.C);
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.C;
	}

	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
	
	/**
	 * Special parse method used just to identify a file as C or not. Will timeout according to config settings.
	 *
	 * @param file the file to parse to see if it is C
	 * @param discoveryCache the {@link DiscoveryJobCache} to store identified headers
	 * @param jobId the Id of the {@link Job}
	 * @param monitor the {@link ProgressMonitor}
	 * @return the {@link Identification} or {@code null}
	 */
	@Nullable
	public Identification parseFileToValidateOnly(final SourcePojo file, final DiscoveryJobCache discoveryCache, final String jobId,
			final ProgressMonitor monitor) {
		final Set<String> headerIncludes = Sets.newConcurrentHashSet(); /* to fill in with header includes in C file */
		final CancellableParserProgressMonitor progressMonitor = new CancellableParserProgressMonitor();
		try {
			/* check if this is a C program */
			final Optional<Identification> result = worker.execute(
					() -> checkIfCProgram(file, headerIncludes, progressMonitor),
					timeout, UNIT, MessageProvider.from(file, ResolveTarget.C)
			);
			if (result.isPresent()) {
				headerIncludes.forEach(header -> discoveryCache.putMultiValue(jobId, CFileTypeDetection.CACHE_KEY, header.toUpperCase()));
				return result.get();
			}
		} catch (final WorkerCancellationException e) {
			progressMonitor.cancel();
			/* explicitly ignore the exception */
			LOG.debug(() -> String.format("[%s] C antlr parser: parsing cancelled due to timeout", file.getName()));
		} catch (final WorkerException exception) {
			progressMonitor.cancel();
			LOG.error(() -> "[C] Antlr Parser not able to assemble/parse file", exception);
		}
		return null;
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
		/* We are excluding the identification of files which are having .cpp and .hpp extension because 
		 * most of the C++ files with above extension can be parsed with below parser and identifying as C files, which is not correct. */
		final var extension = FilenameUtils.getExtension(sourceObject.getPath());
		if ("CPP".equalsIgnoreCase(extension) || "HPP".equalsIgnoreCase(extension)) {
			return null;
		}
		if (phase == DetectionPhase.MAIN) {
			/* check regular C programs (should also get C header files as well) */
			LOG.trace(() -> "[C FileDetection]: categorize by content " + sourceObject.getName());

			return parseFileToValidateOnly(sourceObject, discoveryCache, jobId, monitor);
		}
		return null;
	}

	private Optional<Identification> checkIfCProgram(final SourcePojo file, final Set<String> returnedIncludes, final ParserProgressMonitor monitor ) {
		final var fileDetectionVisitor = new CDiscoveryFileDetectionVisitor();
		fileDetectionVisitor.setErrorLimit(10);
		final var antlrParser = new CAntlrParser()
				.setPreProcessor(new CProgramPreProcessor())
				.setErrorListener(new BaseAntlrErrorListener());
		/* set error limit so we quit parsing after n errors */
		antlrParser.setParserProgressMonitor(monitor);

		try {
			antlrParser.parseTextWithVisitor(file.getContent().toString(), fileDetectionVisitor);
			final Identification identification;
			if (fileDetectionVisitor.isValidProgram()) {
				identification = new Identification(ID.YES, file.getId(), ResolveTarget.C_PROGRAM, ResolveTarget.C);
			} else if (antlrParser.getSyntaxErrors().size() <= FILE_DETECTION_ERROR_LIMIT) {
				identification = new Identification(ID.MAYBE, file.getId(), ResolveTarget.C_PROGRAM, ResolveTarget.C);
			} else {
				identification = new Identification(ID.NO, file.getId(), ResolveTarget.NONE, ResolveTarget.C);
			}
			if (identification.getId() != ID.NO) {
				/* parse includes when identified as a C source */
				final var includeParser = new CIncludeOnlyAntlrParser().setPreProcessor(new CProgramPreProcessor())
						.setErrorListener(new BaseAntlrErrorListener());
				final var headerIncludeVisitor = new CHeaderIncludeVisitor();
				headerIncludeVisitor.setErrorLimit(10);
				includeParser.parseTextWithVisitor(file.getContent().toString(), headerIncludeVisitor);
				returnedIncludes.addAll(headerIncludeVisitor.getIncludeLocalReferences());
			}
			return Optional.of(identification);
		} catch (final Exception e) {
			LOG.debug(() -> "[C] Unable to parse content of [" + file.getName() +  "] because of " + e.getCause() + ", " + e.getMessage(), e);
			LOG.debug(() -> ExceptionUtils.getFullStackTrace(e)); 
		}
		return Optional.empty();
	}
}
