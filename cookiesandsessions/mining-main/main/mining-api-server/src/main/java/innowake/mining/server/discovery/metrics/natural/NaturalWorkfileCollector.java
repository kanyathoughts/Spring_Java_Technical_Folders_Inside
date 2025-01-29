/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.collection.MultiValueMapUtil;
import innowake.lib.parsing.util.visitor.TopDown;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.FileAccess;
import innowake.mining.server.discovery.metrics.natural.NaturalDbAccessCollector.AccessType;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.jcl.parser.api.IDDModelInternal;
import innowake.ndt.jcl.parser.model.JCL;
import innowake.ndt.jcl.parser.model.StepExec;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.ast.Expression;
import innowake.ndt.naturalparser.ast.Expression.StringConstant;
import innowake.ndt.naturalparser.ast.NaturalNode;
import innowake.ndt.naturalparser.ast.NaturalSwitch;
import innowake.ndt.naturalparser.ast.Statement.DefineWorkFileStmt;
import innowake.ndt.naturalparser.ast.Statement.ReadWorkFileStmt;
import innowake.ndt.naturalparser.ast.Statement.WriteWorkFileStmt;

/**
 * Collects all workfile operations.
 */
public class NaturalWorkfileCollector extends NaturalSwitch implements Visitor {
	
	private final IModuleRepository repo;
	private final JclParseResultProvider jclParseResultProvider;
	private final NaturalParseResultProvider naturalParserResultProvider;
	private final ModelArtifact entry;
	private final SourcePojo sourceObject;
	private final Map<Long, DefineWorkFileStmt> defineWorkFileStmts = new HashMap<>();
	private final MultiValuedMap<String, FileAccess> fileAccesses = MultiValueMapUtil.createArrayListHashMap();
	
	/**
	 * Constructor.
	 * 
	 * @param repo the {@link IModuleRepository}
	 * @param entry the {@link ModelArtifact}
	 * @param sourceObject the {@link SourcePojo}
	 * @param jclParseResultProvider the {@link JclParseResultProvider}
	 * @param naturalParserResultProvider the {@link NaturalParseResultProvider}
	 */
	public NaturalWorkfileCollector(final IModuleRepository repo, final ModelArtifact entry, final SourcePojo sourceObject,
			final JclParseResultProvider jclParseResultProvider, final NaturalParseResultProvider naturalParserResultProvider) {
		this.repo = repo;
		this.jclParseResultProvider = jclParseResultProvider;
		this.naturalParserResultProvider = naturalParserResultProvider;
		this.entry = entry;
		this.sourceObject = sourceObject;
	}
	
	/**
	 * Starts collecting the workfile access information.
	 * 
	 * @return mapping from DDM name to {@link AccessType}
	 * @throws DiscoveryException if the Natural module cannot be parsed
	 */
	public MultiValuedMap<String, FileAccess> doCollect() throws DiscoveryException {
		final INaturalModel model = naturalParserResultProvider.getParseResult(sourceObject).getHeavyweightModel();
		new TopDown(this).visit(model.getProgramObject());
		return fileAccesses;
	}
	
	@Override
	public boolean visit(@Nullable final Object node) {
		if (node instanceof NaturalNode) {
			handle(node);
		}
		return true;
	}
	
	@Override
	protected void handleDefineWorkFileStmt(@Nullable final DefineWorkFileStmt node) {
		if (node != null) {
			defineWorkFileStmts.put(Long.valueOf(node.getWorkFileNumber()), node);
		}
	}
	
	@Override
	protected void handleReadWorkFileStmt(@Nullable final ReadWorkFileStmt node) {
		if (node != null) {
			final long number = node.getWorkFile();
			handle(number, FileAccess.READ);
		}
	}

	@Override
	protected void handleWriteWorkFileStmt(@Nullable final WriteWorkFileStmt node) {
		if (node != null) {
			final long number = node.getWorkFile();
			handle(number, FileAccess.WRITE);
		}
	}

	private void handle(final long workFileNumber, final FileAccess fileAccess) {
		final DefineWorkFileStmt dwfs = defineWorkFileStmts.get(Long.valueOf(workFileNumber));
		
		/* first check if there has been a DEFINE WORK FILE statement */
		if (dwfs != null) {
			final Expression workFileName = dwfs.getWorkFileName();
			if (workFileName instanceof StringConstant) {
				final String fileName = FilenameUtils.getName(((StringConstant) workFileName).getStringValue().toString());
				if ( ! fileAccesses.containsMapping(fileName, fileAccess)) {
					fileAccesses.put(fileName, fileAccess);
				}
			}
		} else {
			/* if there's no DEFINE WORK FILE statement, we check any existing batchjob */
			final List<ModelArtifact> jobs = repo.getLanguageEntries(ResolveTarget.JCL);
			jobs.forEach(job -> {
				try {
					final Optional<String> path = job.getPath();
					if (path.isPresent()) {
						final List<JCL> parseResult = jclParseResultProvider.getParseResult(sourceObject).getFull();
						parseResult.stream()
							.map(JCL::getSteps)
							.flatMap(List::stream)
							.filter(step -> step instanceof StepExec)
							.forEach(step -> resolveFromStep((StepExec) step, workFileNumber, fileAccess));
					}
				} catch (final Exception e) {
					/* ignored, as any parse errors will already be handled during jcl discovery */
				}
			});
		}
	}

	private void resolveFromStep(final StepExec step, final long workFileNumber, final FileAccess fileAccess) {
		final String qualifiedStepName = step.getQualifiedStepName();
		final Optional<ModelArtifact> stepEntry = repo.getEntry(entry, qualifiedStepName, ResolveTarget.JCL_EXEC_PGM, ResolveTarget.JCL_EXEC);
		if (stepEntry.isPresent()) {
			stepEntry.get().getDependencies()
				.filter(dependency -> entry.equals(dependency.getTarget()))
				.findFirst()
				.ifPresent(dependency -> {
					/* step executes this module, so try to get dataset definition */
					final Map<String, List<IDDModelInternal>> dds = step.getDDs();
					resolveDDFromStep(dds, Long.valueOf(workFileNumber), fileAccess);
				});
		}
	}

	private void resolveDDFromStep(final Map<String, List<IDDModelInternal>> dds, final Long workFileNumber, final FileAccess fileAccess) {
		/* the dataset can either be defined as CMWKFxx or CMWRKxx */
		List<IDDModelInternal> wkfDds = dds.get(String.format("CMWKF%02d", workFileNumber));
		if (wkfDds == null || wkfDds.isEmpty()) {
			wkfDds = dds.get(String.format("CMWRK%02d", workFileNumber));
		}
		
		if (wkfDds != null && ! wkfDds.isEmpty()) {
			wkfDds.forEach(wkfDd -> {
				if ( ! fileAccesses.containsMapping(wkfDd.getDSN(), fileAccess)) {
					fileAccesses.put(wkfDd.getDSN(), fileAccess);
				}
			});
		}
	}
}
