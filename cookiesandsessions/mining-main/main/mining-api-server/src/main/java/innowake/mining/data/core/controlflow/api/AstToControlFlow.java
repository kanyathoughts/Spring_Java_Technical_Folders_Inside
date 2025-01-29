/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.api;

import java.util.Collection;

import org.apache.commons.lang.NotImplementedException;
import innowake.mining.data.core.controlflow.impl.c.CAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.cobol.CobolAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.java.JavaAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.jcl.JclAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.natural.NaturalAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.pl1.Pl1AstToControlFlowImpl;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;
import innowake.mining.shared.model.Technology;

/**
 * Provides a resolver for AST nodes that calculates control flow.
 */
public interface AstToControlFlow {
	
	/**
	 * Returns a new instance of the default implementation to calculate the control flow.
	 * 
	 * @param technology the programming language
	 * @param rootNode the root node of the AST
	 * 
	 * @return a new instance of the default implementation
	 */
	public static AstToControlFlow getNewInstance(final Technology technology, final AstNodePojo rootNode) {
		switch (technology) {
			case COBOL:
				return new CobolAstToControlFlowImpl(rootNode);
			case NATURAL:
				return new NaturalAstToControlFlowImpl(rootNode);
			case C:
				return new CAstToControlFlowImpl(rootNode);
			case PL1:
				return new Pl1AstToControlFlowImpl(rootNode);
			case JCL:
				return new JclAstToControlFlowImpl(rootNode);
			case JAVA:
				return new JavaAstToControlFlowImpl(rootNode);
			default:
				throw new NotImplementedException("Controlflow calculation for " + technology + " is not supported");
		}
	}
	
	/**
	 * Resolves the control flow
	 */
	void calculateControlFlow();
	
	public Collection<ControlFlowPrototype> getFlow();
	
	public Collection<AstModuleRelationshipPojoPrototype> getTerminals();
	
}
