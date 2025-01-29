/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.branchstatement;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import com.google.gson.Gson;

import innowake.mining.shared.access.AstService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;

/**
 * Branch Statement implementation
 */
@Component
public class BranchStatementImpl implements BranchStatement {

	private final AstNodePojo branchStatement;
	private List<DataDictionaryPojo> dataDictionaryEntries;

	public BranchStatementImpl(final AstNodePojo branchStatement, final List<DataDictionaryPojo> dataDictionaryEntries) {
		super();
		this.branchStatement = branchStatement;
		this.dataDictionaryEntries = dataDictionaryEntries;
	}

	public void setDataDictionaryEntries(final List<DataDictionaryPojo> dataDictionaryEntries) {
		this.dataDictionaryEntries = dataDictionaryEntries;
	}

	@Override
	public List<DataDictionaryPojo> getConditionVariables() {
		return dataDictionaryEntries;
	}

	@Override
	public Map<String, AstNodePojo> getBranches() {
		final Map<String, AstNodePojo> branches = new HashMap<>();
		branchStatement.getOutgoingRelations().stream().filter(r -> r.getType().equals(AstRelationshipType.FLOW))
			.forEach(flowsControl -> {
				final AstNodePojo targetNode = flowsControl.getDstNode();
				branches.put(flowsControl.getLabel().orElseGet(targetNode::getLabel), targetNode);
			});
		return branches;
	}

	@Override
	public List<String> getConditionAsString() {
		return extractConditions().stream().map(AstNodePojo::getLabel).collect(Collectors.toList());
	}

	@Override
	public List<AstNodePojo> getConditions() {
		return extractConditions();
	}

	private List<AstNodePojo> extractConditions() {
		final List<AstNodePojo> astNodes = new ArrayList<>();
		AstService.Properties.CONDITION_PATH.optionalFrom(branchStatement.getProperties()).ifPresent(strPath -> {
			final int[][] conditionPaths = new Gson().fromJson(strPath, int[][].class);
			for (final int[] path : conditionPaths) {
				AstNodePojo currentNode = branchStatement;
				for (final int index : path) {
					final List<AstNodePojo> children = currentNode.getChildren();
					if (index >= 0 && index < children.size()) {
						currentNode = children.get(index);
					} else {
						currentNode = null;
						break;
					}
				}
				if (currentNode != null) {
					final String label = currentNode.getLabel();
					if (label != null && !label.isEmpty()) {
						astNodes.add(currentNode);
					}
				}
			}
		});
		return astNodes;
	}
}
