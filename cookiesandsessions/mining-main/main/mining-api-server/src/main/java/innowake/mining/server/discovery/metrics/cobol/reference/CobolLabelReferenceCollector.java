/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics.cobol.reference;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.lib.core.lang.Nullable;
import innowake.ndt.cobol.parser.ast.CobolAstUtil;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.model.CobolSection;
import innowake.ndt.cobol.parser.ast.model.ProcedureDivision;
import innowake.ndt.cobol.parser.ast.statement.CobolExitStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolGoBackStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolGoToStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolLabelStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolPerformStmt;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Collect Cobol label reference
 */
public class CobolLabelReferenceCollector {

	/**
	 * Locate unreferenced labels in Cobol model
	 * @param model Cobol Model for searching
	 * @return Unreferenced Cobol Label 
	 */
	public static List<CobolLabelStmt> findUnreferencedLables(final CobolModel model) {

		/* find all labels */
		final List<CobolLabelStmt> llist = CobolAstUtil.findChildren(model.getCobolProgram(), CobolLabelStmt.class);
		final Map<String, List<CobolLabelStmt>> labels = llist.stream().collect(Collectors.toMap(l-> buildLabelName(l), 
																						   l -> Arrays.asList(l), 
																						   (l1,l2) -> {
																							   List<CobolLabelStmt> l3 = new ArrayList<>(l1);
																							   l3.addAll(l2);
																							   return l3;
																						   }));
		
		/* final all sections */
		final List<CobolSection> slist = CobolAstUtil.findChildren(model.getCobolProgram(), CobolSection.class);
		final Map<String, CobolSection> sections = slist.stream().collect(Collectors.toMap(CobolSection::getName, s-> s));
		
		/* traverse the model based on its structure */
		final Map<CobolNode, CobolNode> vistitedNodes = new HashMap<>();
		final ProcedureDivision procedureDivision = assertNotNull(model.getCobolProgram().getProcedureDivision());
		visitBlock(model, labels, sections, vistitedNodes, null, procedureDivision,
				false, procedureDivision, null);

		/* find all labels has not been visited*/
		
		return llist.stream().filter(l -> !vistitedNodes.containsKey(l)).collect(Collectors.toList());
	}

	/**
	 * Visit the Cobol model
	 * 
	 * @param model Cobol Model for searching
	 * @param labels labels in the model
	 * @param sections sections in the model
	 * @param vistitedNodes record the nodes has been visited
	 * @param parent parent node of current Cobol node
	 * @param node current Cobol node for visiting
	 * @param calledByPerform true when a target node is CobolFieldReference and appears in the labels or sections list
	 * @param division the program division in the COBOL
	 * @param endNode stop visiting when endNode is reached
	 * 
	 * @return Whether stop the visiting loop at parentLevel
	 */
	private static boolean visitBlock(final CobolModel model, final Map<String, List<CobolLabelStmt>> labels, final Map<String, CobolSection> sections,
			final Map<CobolNode, CobolNode> vistitedNodes, final @Nullable CobolNode parent, final CobolNode node,
			final boolean calledByPerform, final CobolNode division,  final @Nullable CobolNode endNode) {
		/* Whether stop the visiting loop at parentLevel*/
		boolean stopVisiting = false;
		CobolNode currentNode = node;
		CobolNode end = endNode;
		
		if (!vistitedNodes.containsKey(currentNode)) {
			/* record the visited node */
			vistitedNodes.put(currentNode, parent);
			/* within a label we check for all performs */
			if (currentNode instanceof CobolLabelStmt) {
				final List<CobolPerformStmt> plist = CobolAstUtil.findChildren(currentNode, CobolPerformStmt.class);
				
				/* handle perform statements */
				for (final CobolPerformStmt c : plist) {
					end = visitPerformNodes(c, model, labels, sections, vistitedNodes, currentNode, division, end);
				}
			}
			/* Handle the children statements under given node */
			List<AstNode> rootChildren = currentNode.getChildren();
			/* check the type of each child, visit each child node based on its type */
			int i = 0;
			while (i < rootChildren.size()) {
					final AstNode c =  rootChildren.get(i++);
					/* skip the section or labels node when visiting the model, those node should only be visited through reference from other nodes */
					if (calledByPerform) {
						if (c instanceof CobolSection && currentNode instanceof CobolSection) {
							break;
						} else if (c instanceof CobolLabelStmt && currentNode instanceof CobolLabelStmt) {
							break;
						}
					}
					/* skip when find the GoBack statement and stop the loop at parent level */
					if (c instanceof CobolGoBackStmt) {
						stopVisiting = true;
						break;
					}
					
					if (c instanceof CobolExitStmt) {
						break;
					}
					
					if (c instanceof CobolLabelStmt || c instanceof CobolSection) {
						stopVisiting = visitBlock(model, labels, sections, vistitedNodes, currentNode, (CobolNode) c, calledByPerform, division, end);
					}
					if (c instanceof CobolPerformStmt) {
						end = visitPerformNodes((CobolPerformStmt)c, model, labels, sections, vistitedNodes, currentNode, division, end);
					}
					/* check GoTo statements for referencing nodes */
					if (c instanceof CobolGoToStmt) {
						final List<String> targets = ((CobolGoToStmt) c).getTarget();
						for (final String t : targets) {
							/* GOTO section */
							if (sections.get(t) != null) {
								CobolSection s = sections.get(t);
								currentNode = division;
								rootChildren = division.getChildren();
								int index = rootChildren.indexOf(s);
								if(index !=-1) {
									i = index;
									break;
								}
								/* GOTO label */
							} else if (labels.get(t) != null) {
								CobolLabelStmt l = labels.get(t).get(0);
								currentNode = division;
								rootChildren = division.getChildren();
								int index = rootChildren.indexOf(l);
								if(index !=-1) {
									i = index;
									break;
								}
								/* GOTO the fist matching label under sections */
							} else {
								for(final String s: sections.keySet()) {
									if (labels.get(s+"."+t) != null) {
										visitBlock(model, labels, sections, vistitedNodes, node, labels.get(s+"."+t).get(0), true, division, end);
										break;
									} 
								}
							}
						}
					}
					/* Once the stop flag set or reach the end node, stop visiting */
					if(stopVisiting || c.equals(end)) break;
			}
			return stopVisiting;
			
		} else {
			return false;
		}
	}
	
	/**
	 * Find the scope to run the PerformThru statement.
	 * Return the target only in the list if thru is not found.
	 * There are room for improve: prepare a list of linked Node for this method, no need to iterate the DIVISION.
	 * 
	 * @param division the program division in the COBOL
	 * @param target Cobol reference in the perform thru node.
	 * @param thru Cobol reference in the perform thru node.
	 * */
	private static List<CobolNode> collectPerformThruNodes(final CobolNode division, final CobolReference target, final CobolReference thru){
		final List<CobolNode> res = new LinkedList<>();
		final List<AstNode> nodes = division.getChildren();
		boolean record = false;
		for(final AstNode n:nodes) {
			if(n instanceof CobolLabelStmt) {
				if(record) {
					res.add((CobolNode)n);
					final String t = ((CobolFieldReference) thru).getUnresolvedFieldName();
					if(((CobolLabelStmt)n).getName().equals(t)) return res;
				} else {
					final String t = ((CobolFieldReference) target).getUnresolvedFieldName();
					if(((CobolLabelStmt)n).getName().equals(t)) {
						record = true;
						res.add((CobolNode)n);
					}
				}
			}
		}
		return Arrays.asList(target);
	}
	
	/**
	 * Visit the CobolPerformStmt
	 * 
	 * @param c CobolPerformStmt to be visited
	 * @param model Cobol Model for searching
	 * @param labels labels in the model
	 * @param sections sections in the model
	 * @param vistitedNodes record the nodes has been visited
	 * @param node current Cobol node for visiting
	 * @param division the program division in the COBOL
	 * @param endNode stop visiting when endNode is reached
	 * 
	 * @return end node
	 */
	private static @Nullable CobolNode visitPerformNodes(final CobolPerformStmt c, final CobolModel model, final Map<String, List<CobolLabelStmt>> labels,
			final Map<String, CobolSection> sections, final Map<CobolNode, CobolNode> vistitedNodes, final CobolNode node,
			final CobolNode division,  final @Nullable CobolNode endNode) {
		final CobolReference target = c.getTarget();
		final CobolReference thru = c.getThru();
		CobolNode end = endNode;
		if (target instanceof CobolFieldReference) {
			final CobolFieldReference t = (CobolFieldReference) target;
			final String targetName = t.getUnresolvedFieldName();
			/* perform section */
			if (sections.get(targetName) != null) {
				visitBlock(model, labels, sections, vistitedNodes, node, sections.get(targetName), true, division, end);
			} else {
				final String sectionName = t.getOfField() != null ? t.getOfField().getUnresolvedFieldName()+".":"";
				/*perform label*/
				if (labels.get(sectionName+targetName) != null) {
					for( CobolNode l : labels.get(sectionName+targetName) ) {
						visitBlock(model, labels, sections, vistitedNodes, node, l, true, division, end);
					}
				} else {
					/*perform the fist label find in sections*/
					for(final String s: sections.keySet()) {
						if (labels.get(s+"."+targetName) != null) {
							visitBlock(model, labels, sections, vistitedNodes, node, labels.get(s+"."+targetName).get(0), true, division, end);
							break;
						} 
					}
				}
			} 
		}
		
		if (thru != null) {
			if (thru instanceof CobolFieldReference) {
				List<CobolNode> list = collectPerformThruNodes(division, target, thru);
				end = list.get(list.size()-1);
				for(CobolNode l:list) {
					visitBlock(model, labels, sections, vistitedNodes, node, l, true, division, end);
				}
			}
		}
		return end;
	}
	
	/**
	 * Get the label name with section
	 * 
	 * @param label to get name
	 * 
	 * @return label name with section
	 */
	public static String buildLabelName(final CobolLabelStmt label) {
		if(label.getParent() instanceof CobolSection) {
			return ((CobolSection)label.getParent()).getName() + "." + label.getName();
		}
		return label.getName();
	}
	
}
