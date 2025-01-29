/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.candidate;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Identifies BusinessVariableIdentifier for PL1 Modules.
 */
public class Pl1BusinessVariableIdentifier extends BusinessVariableIdentifier {
	
	/**
	 * The Constructor.
	 * 
	 * @param rootNode the root AST node of the Module
	 * @param ddeNameMap a map of {@linkplain DataDictionaryPojo} dataElementName to the {@linkplain DataDictionaryPojo} entries
	 */
	public Pl1BusinessVariableIdentifier(final @Nullable AstNodePojo rootNode, final Map<String, List<DataDictionaryPojo>> ddeNameMap) {
		super(rootNode, ddeNameMap);
	}

	@Override
	protected List<AstNodePojo> getArithmeticStatements() {
		return ASSIGNMENT_COLLECTOR.allDeep(Assert.assertNotNull(getRootNode())).stream()
				.filter(ast -> ast.getChildren().stream().anyMatch(node -> AstNodeUtils.hasAnySuperType(node, AstNodeUtils.ARITHMETIC_EXPRESSION)))
				.collect(Collectors.toList());		
	} 

}
