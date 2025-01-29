/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.batch.DiscoveryJclContentProvider;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.jcl.parser.model.JCL;
import innowake.ndt.jcl.parser.model.Step;
import innowake.ndt.jcl.parser.model.StepExec;
import innowake.ndt.jcl.parser.model.StepIf;

/**
 * Contributor for JCL_PROC files.
 */
@Component
public class JclProcContributor extends AbstractJclContributor {
	private static final Logger LOG = LoggerFactory.getLogger(JclProcContributor.class);
	
	@Autowired
	private SourceService sourceService;
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.JCL && sourceObject.getType() == Type.PROC;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.JCL_PROC);
		calculateSourceMetrics(rootModule, sourceObject, 0);
		if (sourceObject.getContent().toString().isBlank()) {
			rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
			return;
		}
		
		try {
			final var jclContentProvider = new DiscoveryJclContentProvider(context.getProjectId(), sourceService, context.getSearchOrders(),
					sourceObject.getPath());
			final var jclParseResult = getParseResult(sourceObject, context, jclContentProvider);
			final List<JCL> listOfJcls = jclParseResult.getFull();
			collectSteps(builder, listOfJcls);
		} catch (final DiscoveryException e) {
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
		} catch (final WorkerCancellationException e) {
			LOG.error("Parser timeout error occured while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_TIMEOUT, e.getMessage());
			/* unable to parse so nothing more to discover */
		} catch (final Exception e) {
			LOG.error("Exception occurred while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		} catch (final Throwable e) {
			LOG.error("Unexpected error occurred while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}
	}

	private void collectSteps(final DiscoveryBuilderFromSource builder, final List<JCL> listOfJcls) {
		if ( ! listOfJcls.isEmpty()) {
			final List<Step> steps = listOfJcls.stream().flatMap(JobControl::getJCLStepsRecursively)
					.filter(step -> step instanceof StepExec || step instanceof StepIf).collect(Collectors.toList());
			final Set<String> stepsWithinSameSource = new HashSet<>();
			steps.forEach(step -> collectStepModules(step, builder, stepsWithinSameSource));
		}
	}
	
	private void collectStepModules(final Step step, final DiscoveryBuilderFromSource builder, final Set<String> stepsWithinSameSource) {
		if (step instanceof StepExec) {
			final StepExec execStep = (StepExec) step;
			final String qualifiedStepName = execStep.getQualifiedStepName();
			if (stepsWithinSameSource.add(qualifiedStepName) && ! execStep.isInsideInlinedProc()) {
				builder.declareSubModule(qualifiedStepName, JobControl.getExecType(execStep));
			}
		} else if (step instanceof StepIf) {
			JobControl.getIfStepChildSteps((StepIf) step).forEach(childStep -> collectStepModules(childStep, builder, stepsWithinSameSource));
		}
	}
}
