/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.ndt.parsing.parser.dependency.ast.pl1.Pl1ProcedureNode;

/**
 * Pl1 contributor that uses the lightweight parser and Pl1ProcedureNode.
 */
public class Pl1ExperimentalLightweightContributor extends AbstractPl1Contributor<Pl1ProcedureNode>{

	Pl1ExperimentalLightweightContributor(final Pl1ParseResultProvider.Pl1ParseResult parseResult,
			final Pl1DependencyUtility<Pl1ProcedureNode> pl1DependencyUtility, final Config config) throws DiscoveryException {
		super(parseResult, pl1DependencyUtility, config);
	}

	@Override
	Class<Pl1ProcedureNode> getProcedureClass() {
		return Pl1ProcedureNode.class;
	}
}
