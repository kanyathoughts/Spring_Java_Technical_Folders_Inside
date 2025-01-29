/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import innowake.ndt.core.parsing.ast.visitor.Visitor;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Top to down visitor for {@link AstNode}s that visits a custom visitor.
 */
public class AstVisitor implements Visitor<AstNode> {

	private final Visitor<AstNode> visitor;
	
    /**
     * Creates an instance.
     * 
     * @param visitor the custom visitor
     */
    public AstVisitor(final Visitor<AstNode> visitor) {
    	this.visitor = visitor;
    }
    
	@Override
	public boolean visit(final AstNode node) {
        boolean result = visitor.visit(node);
        if (result) {
        	for (final AstNode child : node.getChildren()) {
        		result &= visit(child);
        		if ( ! result) {
        			break;
        		}
        	}
        }
        return result;
	}

}
