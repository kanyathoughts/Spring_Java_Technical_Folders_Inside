/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.core.CandidateIdentificationResult;
import innowake.mining.data.core.candidate.CandidateIdentifier;
import innowake.mining.data.core.controlflow.ControlFlowGraph;
import innowake.mining.data.core.moduledescription.ModuleDescriptionIdentifier;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.data.core.taxonomy.TechnicalTaxonomyIdentifier;
import innowake.mining.data.error.Exceptions;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.job.NullParsingSummarizer;
import innowake.mining.shared.model.job.ParsingSummarizer;

/**
 * Provides entry points to execute various operations like storeAst against orientDb. All operations are executed transactional.
 */
@Service
@Transactional("postgres")
public class ExecutorService {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.DATA);
	
	@Autowired
	private MiningDataCoreService core;
	@Autowired
	private TechnicalTaxonomyIdentifier technicalTaxonomyIdentifier;

	private ParsingSummarizer summarizer;
	
	/**
	 * Creates an instance of the {@link ExecutorService} that does not summarize the results.
	 */
	public ExecutorService() {
		this(NullParsingSummarizer.INSTANCE);
	}
	
	/**
	 * Creates an instance of the {@link ExecutorService} that will allow for summarizing the results.
	 * @param summarizer the instance that will record the AST operations for summarization.
	 */
	public ExecutorService(final ParsingSummarizer summarizer) {
		this.summarizer = summarizer;
	}
	
	/**
	 * Sets the summarizer.
	 * @param summarizer will summarize the results of the operation.
	 */
	public void setParsingSummarizer(final ParsingSummarizer summarizer) {
		this.summarizer = summarizer;
	}
	
	/**
	 * Executes the storeAst operation if not already present in the database.
	 *
	 * @param projectId the Id of the project
	 * @param moduleId the Id of the module
	 * @return {@code true} if the AST is either already present in the database or has been successfully stored; {@code false} otherwise
	 */
	public boolean executeStoreAst(final EntityId projectId, final EntityId moduleId) {
		try {
			/* only store ast if there are currently no nodes present. */
			boolean hasAstNodes = core.astService.count(q -> q.ofProject(projectId).ofModule(moduleId)) > 0l;
			if ( ! hasAstNodes) {
				hasAstNodes = new DefaultStoreAstExecutor(summarizer).execute(moduleId, core).isPresent();
			}
			return hasAstNodes;
		} catch (final UncategorizedSQLException e) {
			Exceptions.checkForEmptyResultDataAccessException(e, String.class, moduleId.toString());
			throw e;
		}
	}
	
	/**
	 * Executes the technical taxonomy identification for the module with the provided Id.
	 *
	 * @param moduleId the Id of the module to process
	 * @return the {@link CandidateIdentificationResult} or {@code null}
	 */
	public CandidateIdentificationResult executeTechnicalTaxonomyIdentification(final EntityId moduleId) {
		try {
			return technicalTaxonomyIdentifier.identify(moduleId, new DefaultStoreAstExecutor(summarizer), core);
		} catch (final Exception e) {
			LOG.error(() -> "Error during identifying Technical Taxonomies for the module with Id " + moduleId, e);
			throw e;
		}
	}
	
	/**
	 * Executes the candidate identification for the module with the provided Id.
	 *
	 * @param moduleId the Id of the module to process
	 * @param jobId the Id of the job of the candidate identification
	 * @param identifyOnlyDDE boolean to run identification of DDE only and skip annotation identification
	 * @return the {@link CandidateIdentificationResult} or {@code null}
	 */
	@Nullable
	public CandidateIdentificationResult executeCandidateIdentification(final EntityId moduleId, final String jobId, final boolean identifyOnlyDDE) {
		try {
			return new CandidateIdentifier(moduleId, new DefaultStoreAstExecutor(summarizer), core, jobId)
					.identify(identifyOnlyDDE);
		} catch (final Exception e) {
			LOG.error(() -> "Error during identifying candidates for the module with Id " + moduleId, e);
			throw e;
		}
	}
	
	/**
	 * Executes the module description identification for the module with the provided Id.
	 * 
	 * @param moduleId the Id of the module to process
	 * @return the result of the module identification
	 */
	@Nullable
	public Object executeModuleIdentification(final EntityId moduleId) {
		try {
			return ModuleDescriptionIdentifier.identify(moduleId, core.moduleService);
		} catch (final Exception e) {
			LOG.error(() -> "Error during identifying candidates for the module with Id " + moduleId.toString(), e);
			throw e;
		}
	}
	
	/**
	 * Executes the control flow graph calculation for the module with the provided Id.
	 *
	 * @param moduleId the Id of the module to process
	 * @return {@code true} if the control flow has been calculated; {@code false} otherwise
	 */
	public boolean executeControlFlowCalculation(final EntityId moduleId) {
		try {
			return ControlFlowGraph.calculate(moduleId, new DefaultStoreAstExecutor(summarizer), core);
		} catch (final Exception e) {
			LOG.error(() -> "Error during control flow graph calculation for the module with Id " + moduleId, e);
			throw e;
		}
	}
}
