/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.hello;

import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.ModuleLocation;

import java.util.List;

/**
 * Simple contributor that creates a dependency. Only runs on the "DEPTEST" source file.
 */
public class HelloDependencyContributor implements DiscoveryContributorFromSource {

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getName().equals("DEPTEST");
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));

		DiscoveryBuilder.DependencyBuilder copyTestDependencyBuilder = rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("COPYTEST").setTypes(ModuleType.COBOL_COPYBOOK),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL).setLocation(new ModuleLocation(10, 15));
		copyTestDependencyBuilder.setBinding(Binding.LATE);
		copyTestDependencyBuilder.addAttribute(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE, ModelAttributeValue.SendReceiveAccess.RECEIVE);
		copyTestDependencyBuilder.addAttribute(ModelAttributeKey.CALL_TYPE, ModelAttributeValue.CallType.EXECICSCREATE);
		/* This dependency should not be present in sheet */
		rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("Missing3").setTypes(ModuleType.COBOL_COPYBOOK),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL);
		rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("MissingModule1").setTypes(ModuleType.COBOL_COPYBOOK));
		rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("NNMDTCLS").setTypes(ModuleType.UNKNOWN_UTILITY));
		rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("Missing1", "Missing2").setTypes(ModuleType.COBOL_COPYBOOK));
		
		/* This declaration test for correct dependency is chosen based on search order */
		rootModule.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("foo").setTypes(ModuleType.COBOL_COPYBOOK),
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		/* This would create a missing dependency for table if it doesn't exist, or create a dependency to the table */
		rootModule.declareDependency(RelationshipType.ACCESSES, new ModuleFilter().setNames("TABLE_1"), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR);
		if (sourceObject.getName().equals("DEPTEST")) {
			rootModule.deferAction("sampleMethod");
			/* Tests the multiple filters functionality */
			final var filter1 = new ModuleFilter().setNames("MFILTERT").setTypes(ModuleType.JCL_CONTROLCARD);
			final var filter2 = new ModuleFilter().setPaths("src/jcl/dependency-creation/CARDS/controlcards/MFILTERT.crd");
			rootModule.declareDependency(RelationshipType.REFERENCES, List.of(filter1, filter2));
		}
	}

	@DeferredAction
	public void sampleMethod(final ModuleBuilder moduleBuilder) {
		moduleBuilder.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("COPYTEST2").setTypes(ModuleType.COBOL_COPYBOOK));
		moduleBuilder.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("MissingModule2").setTypes(ModuleType.COBOL_COPYBOOK));
	}
}
