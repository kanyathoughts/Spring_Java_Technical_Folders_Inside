/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.hello;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.AnchorToBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;

/**
 * Contributor that creates default Modules if missing and updates it if present. It also shows on how to reset/update the ModuleType of the module
 */
public class HelloContributorForDefaultModules implements DiscoveryContributorFromSource {

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getName().startsWith("DefaultPrg");
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModuleBuilder = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));

		/* Tests creation of missing default module through separated declareDependency and anchorTo */
		rootModuleBuilder.declareDependency(RelationshipType.INCLUDES,
				new ModuleFilter().setNames("MMRS7101SQLTABLE").setTypes(ModuleType.SQL_TABLE, ModuleType.SQL_VIEW));

		builder.anchorTo(new ModuleFilter().setNames("MMRS7101SQLTABLE").setTypes(ModuleType.SQL_TABLE, ModuleType.SQL_VIEW))
				.createIfMissing("MMRS7101SQLTABLE", ModuleType.SQL_TABLE);

		/* Tests creation of missing default module while declaring the dependency */
		rootModuleBuilder
				.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("TESTPRGASQLTABLE").setTypes(ModuleType.SQL_TABLE, ModuleType.SQL_VIEW))
				.createIfMissing("TESTPRGASQLTABLE", ModuleType.SQL_TABLE);

		final AnchorToBuilder anchorTo = builder.anchorTo(new ModuleFilter().setNames("DefaultPrg"));
		anchorTo.addDeadCode("ANCHOR added label from " + sourceObject.getName(), 333, 444);
		/* Tests the reset type by resetting the Pl1 main program to subroutine */
		anchorTo.andResetType(ModuleType.PL1_SUBROUTINE);

		/* Test to check if the missing modules (-1) is still creating if "orCreateIfMissing" is not set*/
		rootModuleBuilder.declareDependency(RelationshipType.INCLUDES,
				new ModuleFilter().setNames("hn_tree_SQLTABLE").setTypes(ModuleType.SQL_TABLE, ModuleType.SQL_VIEW));

	}

}
