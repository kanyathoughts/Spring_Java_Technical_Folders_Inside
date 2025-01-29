/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.branchstatement;

import java.util.List;
import java.util.Map;

import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * a BranchStatement interface to return information about condition, branches and variables of an astNode having super type BranchStatement
 */

public interface BranchStatement {

	/**
	 * Returns the list of Data Dictionary entries for all variables that are accessed in the condition.
	 *
	 * @return the list of Data Dictionary entries for all variables that are accessed in the condition.
	 */
	List<DataDictionaryPojo> getConditionVariables();

	/**
	 * Return the map of label and first AstNode in the corresponding branch for all outgoing FlowsControl edges. 
	 * </p>
	 * The key of the Map is the label of the FlowsControl edge (e.g. "TRUE" or "FALSE" in case of an if/else statement) 
	 * and the value is the target AstNode of the FlowsControl edge, i.e. the first AstNode in the corresponding branch.
	 *
	 * @return the map of label and first AstNode in the corresponding branch for all outgoing FlowsControl edges.
	 */
	Map<String, AstNodePojo> getBranches();

	/**
	 * Returns the list of the condition as string. It will return the label of AstNode of the condition of the branch statement.
	 *
	 * @return the list of the condition as string.
	 */
	List<String> getConditionAsString();

	/**
	 * Returns the AstNodes (or the root AstNode) of the condition of the branch statement.
	 *
	 * @return the list of the AstNodes (or the root AstNode) of the condition of the branch statement.
	 */
	List<AstNodePojo> getConditions();
}
