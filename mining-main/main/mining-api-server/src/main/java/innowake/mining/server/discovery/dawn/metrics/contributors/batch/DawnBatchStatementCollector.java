/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.AnchorToBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.jcl.parser.api.IDDModelInternal;
import innowake.ndt.jcl.parser.model.JCL;
import innowake.ndt.jcl.parser.model.StepContainer;
import innowake.ndt.jcl.parser.model.StepExec;

/**
 * This class collects jcl statements of interest from given {@link JCL}s.
 */
public class DawnBatchStatementCollector {

	private static final Pattern LF_PATTERN = Pattern.compile("\\r?\\n");
	private final List<JCL> jcls;
	private final ModuleBuilder rootModuleBuilder;
	private final DiscoveryBuilderFromSource builder;

	public DawnBatchStatementCollector(final List<JCL> jcls,
			final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModuleBuilder) {
		this.jcls = jcls;
		this.rootModuleBuilder = rootModuleBuilder;
		this.builder = builder;
	}

	/**
	 * Collects the conditionals statements from job
	 *
	 * @param statements the set of statements to avoid duplicate statement creation
	 */
	public void collectStatementsFromJob(final Set<String> statements) {
		jcls.stream().map(JCL::getSteps).map(JobControl::getAllStepIf).flatMap(List::stream).forEach(s -> {
			final String text = "IF " + s.getCond();
			if (statements.add(text)) {
				rootModuleBuilder.declareStatement(StatementType.CONDITIONAL).setText(text).setTechnology(Technology.JCL);
			}
		});
	}

	/**
	 * Collects the exec statements from steps
	 *
	 * @param step the job step
	 * @param stepModuleBuilder the step module builder
	 * @param stepStatements the set of statements to avoid duplicate statement creation
	 * @param resolvedProcs resolved procs map
	 */
	public void collectStatementsFromSteps(final StepExec step, final ModuleBuilder stepModuleBuilder, final Set<String> stepStatements,
			final Map<String, String> resolvedProcs) {
		if (step.isInsideInlinedProc()) {
			final String qualifiedStepName = step.getQualifiedStepName();
			final var procName = qualifiedStepName.substring(0, qualifiedStepName.indexOf('.'));
			final var resolvedProcPath = resolvedProcs.get(procName);
			final ModuleFilter procFilter = new ModuleFilter().setNames(procName).setTypes(ModuleType.JCL_PROC);
			if (resolvedProcPath != null) {
				procFilter.setPaths(resolvedProcPath);
			}
			final AnchorToBuilder procModuleBuilder = builder
					.anchorTo(procFilter);
			final var statementString = getStatementString(step, procModuleBuilder);
			if (stepStatements.add(statementString)) {
				procModuleBuilder.declareStatement(getStatementType(step)).setText(statementString).setTechnology(Technology.JCL);
			}
		} else {
			final var statementString = getStatementString(step, rootModuleBuilder);
			if (stepStatements.add(statementString)) {
				rootModuleBuilder.declareStatement(getStatementType(step)).setText(statementString).setTechnology(Technology.JCL);
			}
		}
	}

	private static StatementType getStatementType(final StepExec exec) {
		if (exec.isExecProc()) {
			return StatementType.EXEC_PROC;
		} else if (exec.isExecREXX()) {
			return StatementType.EXEC_REXX;
		}

		return StatementType.EXEC;
	}

	private static String getStatementString(final StepExec stepExec, final ModuleBuilder rootModuleBuilder) {
		final var props = toString(stepExec.getProperties());

		final String dds = stepExec.getDDs().entrySet().stream().flatMap(entry -> entry.getValue().stream()).map(DawnBatchStatementCollector::getDDModelString)
				.collect(Collectors.joining(","));

		final Map<String, String> additionalProperties = new HashMap<>(3);
		additionalProperties.put("Fully Qualified Step name", stepExec.getFullyQualifiedId());
		additionalProperties.put("Step name", stepExec.getId());

		final StepContainer job = getJcl(stepExec);
		if (job instanceof JCL) {
			final var jcl = (JCL) job;
			if (jcl.getId() != null) {
				additionalProperties.put("Job name", jcl.getId());
			}
		} else {
			final String errorMsg = String.format("Unable to determine the job name for the step: %s. Statement metrics will be incomplete.", stepExec.getId());
			rootModuleBuilder.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR, errorMsg);
		}

		return LF_PATTERN
				.matcher(String.format("Properties: [%s], Data Definitions: [%s], Additional Properties: [%s]", props, dds, toString(additionalProperties)))
				.replaceAll("");
	}

	@Nullable
	private static StepContainer getJcl(@Nullable final StepContainer stepContainer) {
		if (stepContainer instanceof JCL) {
			return stepContainer;
		}
		if (stepContainer == null || stepContainer.getParent() == null) {
			return null;
		}
		return getJcl(stepContainer.getParent());
	}

	private static String getDDModelString(final IDDModelInternal model) {
		final var props = toString(model.getProperties());
		return String.format("%s{%s}", model.getId(), props);
	}

	private static String toString(final Map<String, String> properties) {
		return properties.entrySet().stream().map(entry -> entry.getKey() + "=" + entry.getValue()).collect(Collectors.joining(","));
	}
	
	static class Statement implements Serializable {
		private final ModuleFilter moduleFilter;
		private final String statementString;
		private final String statementType;
		
		public Statement(final ModuleFilter moduleFilter, final String statementString, final String statementType) {
			this.moduleFilter = moduleFilter;
			this.statementString = statementString;
			this.statementType = statementType;
		}
		
		public ModuleFilter getModuleFilter() {
			return moduleFilter;
		}
	
		public String getStatementString() {
			return statementString;
		}

		public String getStatementType() {
			return statementType;
		}
	}
}
