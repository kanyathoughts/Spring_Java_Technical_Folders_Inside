/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;

/**
 * Pl1 contributor that uses the lightweight parser.
 */
public class Pl1LightWeightContributor extends AbstractPl1Contributor<AbstractPl1ProcedureNode>{

	Pl1LightWeightContributor(final Pl1ParseResultProvider.Pl1ParseResult parseResult,
			final Pl1DependencyUtility<AbstractPl1ProcedureNode> pl1DependencyUtility, final Config config) throws DiscoveryException {
		super(parseResult, pl1DependencyUtility, config);
	}

	@Override
	Class<AbstractPl1ProcedureNode> getProcedureClass() {
		return AbstractPl1ProcedureNode.class;
	}
}
