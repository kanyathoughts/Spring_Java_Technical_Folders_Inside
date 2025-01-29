/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.hello;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Simple "from source" contributor that creates a Module for each source file.
 */
public class HelloContributorFromSource implements DiscoveryContributorFromSource {

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return true;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		ModuleBuilder rootModuleBuilder = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));
		rootModuleBuilder.addDeadCode("LABEL", 11, 22);

		final SourceMetrics sourceMetrics = new SourceMetrics();
		sourceMetrics.setCodeLines(42);
		sourceMetrics.setCommentLines(43);
		sourceMetrics.setPhysicalLines(44);
		sourceMetrics.setDeadCodeLines(45);
		rootModuleBuilder.addAdditionalInfo(sourceMetrics);

		if (sourceObject.getName().equals("TESTPRGA")) {
			ModuleBuilder anchorTo = builder.anchorTo(new ModuleFilter().setNames("TESTPRGA"));
			anchorTo.addDeadCode("ANCHOR added label", 333, 444);
			rootModuleBuilder.deferAction("addConditionalDependency");
			builder.declareSubModule("PRGASUB", ModuleType.PL1_SUBROUTINE);
		}

		if (sourceObject.getName().equals("hn_tree")) {
			rootModuleBuilder.deferAction("sampleMethod");
			builder.declareSubModule("hn_treeSub", ModuleType.PL1_SUBROUTINE);
			/* this is to check that the dependency is resolved to the parent of PRGASUB, i.e. TESTPRGA. */
			rootModuleBuilder.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames("PRGASUB").setTypes(ModuleType.PL1_SUBROUTINE),
					ResolutionFlag.RESOLVE_TO_PARENT).setBinding(Binding.EARLY);
		}
		
		if (sourceObject.getName().equals("MMRS7101")) {
			/* This is to check the process of external Modules creation, dependency resolution, anchorTo, createIfMissing based on 
			 * RESOLVE_CASE_INSENSITIVE resolution flag 
			 * */
			final ModuleBuilder externalModule1 = builder.declareExternalModule("test1", ModuleType.RESOURCE_FILE);
			final ModuleBuilder externalModule2 = builder.declareExternalModule("TEST1", ModuleType.RESOURCE_FILE);
			builder.declareExternalModule("test2", ModuleType.RESOURCE_FILE, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
			builder.declareExternalModule("TEST2", ModuleType.RESOURCE_FILE, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
			rootModuleBuilder.declareDependency(RelationshipType.ACCESSES, externalModule1)
							 .setBinding(Binding.EARLY);
			rootModuleBuilder.declareDependency(RelationshipType.ACCESSES, externalModule2)
							 .setBinding(Binding.EARLY);
			rootModuleBuilder.declareDependency(RelationshipType.ACCESSES, 
												new ModuleFilter().setNames("test2").setTypes(ModuleType.RESOURCE_FILE),
												ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
							 .setBinding(Binding.EARLY);
			/* This dependency does not find any parent, it should not be created as missing */
			rootModuleBuilder.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames("test2").setTypes(ModuleType.RESOURCE_FILE),
					ResolutionFlag.RESOLVE_TO_PARENT);
			/* Multiple match error should be thrown if multiple modules are found for RESOLVE_TO_PARENT */
			rootModuleBuilder.declareDependency(RelationshipType.CALLS, new ModuleFilter().setTypes(ModuleType.PL1_SUBROUTINE),
					ResolutionFlag.RESOLVE_TO_PARENT);
			rootModuleBuilder.declareDependency(RelationshipType.ACCESSES,
												new ModuleFilter().setNames("TeSt2").setTypes(ModuleType.RESOURCE_FILE),
												ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
							 .setBinding(Binding.EARLY)
							 .createIfMissing("TeSt2", ModuleType.RESOURCE_FILE);

			rootModuleBuilder.declareDependency(RelationshipType.ACCESSES, 
												new ModuleFilter().setNames("TeSt2").setTypes(ModuleType.RESOURCE_FILE))
							 .setBinding(Binding.EARLY)
							 .createIfMissing("TeSt2", ModuleType.RESOURCE_FILE);
			
			rootModuleBuilder.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("SQLCA").setTypes(ModuleType.COBOL_COPYBOOK))
			.setBinding(Binding.EARLY)
			.createIfMissing("SQLCA", ModuleType.COBOL_COPYBOOK, Origin.ENVIRONMENT);
			
			builder.anchorTo(new ModuleFilter().setNames("test1"))
			.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, "First error to validate anchorTo without RESOLVE_CASE_INSENSITIVE flag");
			
			builder.anchorTo(new ModuleFilter().setNames("TEST1"))
			.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, "Second error to validate anchorTo without RESOLVE_CASE_INSENSITIVE flag");
			
			builder.anchorTo(new ModuleFilter().setNames("test2"), ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
			.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, "Third error to validate anchorTo with RESOLVE_CASE_INSENSITIVE flag");
			
			builder.anchorTo(new ModuleFilter().setNames("test2"))
			.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, "Fourth error to validate anchorTo with RESOLVE_CASE_INSENSITIVE flag");
		}

	}

	@DeferredAction
	public void sampleMethod(final ModuleBuilder moduleBuilder) {
		moduleBuilder.addDeadCode("DEFERRED ACTION added label", 5555, 6666);
		/* update source metrics to different value - these are the final values that should show up in the expected sheet
		 * for the "hn_tree" Module */
		final SourceMetrics sourceMetrics = new SourceMetrics();
		sourceMetrics.setCodeLines(47);
		sourceMetrics.setCommentLines(49);
		sourceMetrics.setPhysicalLines(50);
		sourceMetrics.setDeadCodeLines(89);
		moduleBuilder.addAdditionalInfo(sourceMetrics);
	}

	@SuppressWarnings("unused")
	@DeferredAction
	public void addConditionalDependency(final DiscoveryContext context, final ModuleBuilder moduleBuilder) {
		moduleBuilder.declareDependency(
							RelationshipType.ACCESSES,
							new ModuleFilter().setNames("conditionalModule").setTypes(ModuleType.RESOURCE_FILE))
						.setBinding(Binding.EARLY)
						.createIfMissing("conditionalModule", ModuleType.RESOURCE_FILE)
						.setReachedFromModules(new ModuleFilter().setNames("MMRS7101").setTypes(ModuleType.COBOL_PROGRAM));
	}
}
