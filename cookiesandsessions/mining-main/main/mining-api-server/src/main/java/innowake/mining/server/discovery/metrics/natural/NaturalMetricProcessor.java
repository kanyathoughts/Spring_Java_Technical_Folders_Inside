/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics.natural;

import java.util.HashSet;
import java.util.Set;

import innowake.lib.core.lang.Nullable;
import innowake.lib.parsing.util.visitor.TopDown;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.INaturalModel.ProgramState;
import innowake.ndt.naturalparser.NaturalObjectType;
import innowake.ndt.naturalparser.ast.DecideForClause;
import innowake.ndt.naturalparser.ast.DecideOnClause;
import innowake.ndt.naturalparser.ast.SessionParameter;
import innowake.ndt.naturalparser.ast.Statement.IfSelectionStmt;
import innowake.ndt.naturalparser.ast.Statement.IfStmt;
import innowake.ndt.naturalparser.ast.imp.ImpInputElement.InputField;

/**
 * Collect the metrics for one Natural module.
 */
class NaturalMetricProcessor {

	ModelArtifact calculate(final INaturalModel model, ModelArtifact artifact) {
		if (model.getProgramState() != ProgramState.UNRECOVERABLE_ERRORS) {
			final AstVisitor visitor;
			
			/* added this so we can calculate complexity differently for different types */
			if( model.getNaturalObjectType() == NaturalObjectType.TYPE_MAP ) {
				visitor = new MapAstVisitor();
			} else {
				visitor = new ProgramAstVisitor();
			}
			
			new TopDown(visitor).visit(model.getProgramObject());
			artifact.setComplexity(visitor.complexity);
		} else {
			artifact.setComplexity(-1);
		}
		
		return artifact.validate();
	}

	private class AstVisitor implements Visitor {

		protected int complexity = 1;
		
		@Override
		public boolean visit(@Nullable final Object node) {
			return true;
		}
	}

	private class ProgramAstVisitor extends AstVisitor {

		@Override
		public boolean visit(@Nullable final Object node) {

			if (node instanceof IfStmt ||
				node instanceof IfSelectionStmt ||
				node instanceof DecideForClause ||
				node instanceof DecideOnClause ) {
				complexity++;
			}

			return true;
		}
	}

	private class MapAstVisitor extends AstVisitor {
		
		private static final int FIELD_COMP_VALUE = 1;
		private static final int SESSION_PARAM_COMP_VALUE = 1;
		
		protected Set<Class<?>> parameters = new HashSet<>();

		@Override
		public boolean visit(@Nullable final Object node) {
			if( node instanceof InputField ) {
				
				complexity += FIELD_COMP_VALUE;
			}
			
			else if( node instanceof SessionParameter ) {
				int size = parameters.size();
				parameters.add(node.getClass());
				if( size != parameters.size() ) {
					complexity += SESSION_PARAM_COMP_VALUE; /* add for each unique parameter used */
				}
			} 
			
			return true;
		}
	}

}
