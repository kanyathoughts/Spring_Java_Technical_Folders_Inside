package fw2.hhs.rules;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import fw2.hhs.decisiontables.ActionColumn;
import fw2.hhs.decisiontables.ConditionColumn;
import fw2.hhs.decisiontables.DecisionTable;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.hhs.rules.Variable;

public class RuleManager {

	public boolean createProject(String repository, String projectName, String[] dependentProjects) {

		try {
			File dir = new File(repository + "\\" + projectName);
			dir.mkdirs();
			File projectFile = new File(repository + "\\" + projectName + "\\" + ".project");

			PrintStream ps = new PrintStream(projectFile);
			ps.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
			ps.println("<projectDescription>");
			ps.println("<name>");
			ps.println(projectName);
			ps.println("</name>");
			ps.println("<comment></comment>");
			ps.println("<projects>");

			for (String dependentProject : dependentProjects) {
				ps.println("<project>" + dependentProject + "</project>");
			}
			ps.println("</projects>");
			ps.println("<buildSpec>");
			ps.println("<buildCommand>");
			ps.println("<name>ilog.rules.studio.model.ruleBuilder</name>");
			ps.println("<arguments>");
			ps.println("</arguments>");
			ps.println("</buildCommand>");
			ps.println("</buildSpec>");
			ps.println("<natures>");
			ps.println("<nature>ilog.rules.studio.model.decisionProject</nature>");
			ps.println("<nature>ilog.rules.studio.model.operationProject</nature>");
			ps.println("<nature>ilog.rules.studio.model.ruleNature</nature>");
			ps.println("</natures>");
			ps.println("</projectDescription>");
			ps.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return true;
	}

	public boolean createRuleProject(String repository, String projectName, String[] dependentProjects) {

		File dir = new File(repository + "\\" + projectName);
		dir.mkdirs();

		File ruleDir = new File(repository + "\\" + projectName + "\\rules");
		ruleDir.mkdirs();
		File bomDir = new File(repository + "\\" + projectName + "\\bom");
		bomDir.mkdirs();
		File templatesDir = new File(repository + "\\" + projectName + "\\templates");
		templatesDir.mkdirs();
		File queriesDir = new File(repository + "\\" + projectName + "\\queries");
		queriesDir.mkdirs();

		File projectFile = new File(repository + "\\" + projectName + "\\" + ".ruleproject");

		PrintStream ps;
		try {
			ps = new PrintStream(projectFile);
			ps.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
			ps.println(
					"<ilog.rules.studio.model.base:RuleProject xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:ilog.rules.studio.model.base=\"http://ilog.rules.studio/model/base.ecore\" xmlns:ilog.rules.studio.model.bom=\"http://ilog.rules.studio/model/bom.ecore\" xmlns:ilog.rules.studio.model.query=\"http://ilog.rules.studio/model/query.ecore\" xmlns:ilog.rules.studio.model.rule=\"http://ilog.rules.studio/model/rule.ecore\" xmlns:ilog.rules.studio.model.xom=\"http://ilog.rules.studio/model/xom.ecore\">");
			ps.println("<name>");
			ps.println(projectName);
			ps.println("</name>");
			ps.println("<uuid>" + UUID.randomUUID() + "</uuid>");
			ps.println("<outputLocation>output</outputLocation>");
			ps.println("<categories>any</categories>");

			ps.println(" <modelFolders xsi:type=\"ilog.rules.studio.model.base:SourceFolder\">");
			ps.println("		    <name>rules</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.bom:BOMFolder\">");
			ps.println("<name>bom</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.rule:TemplateFolder\">");
			ps.println("<name>templates</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.query:QueryFolder\">");
			ps.println("<name>queries</name>");
			ps.println("</modelFolders>");

			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.base:ResourceFolder\">");
			ps.println("<name>resources</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"com.ibm.rules.studio.model.decisionservice:OperationFolder\">");
			ps.println("<name>deployment</name>");
			ps.println("</modelFolders>");

			ps.println("</ilog.rules.studio.model.base:RuleProject>");
			ps.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	public void createBOM(String repository, String projectName, String bomName, BOMClass[] classHolderList) {

		try {
			File dir = new File(repository + "\\" + projectName);
			dir.mkdirs();
			File bomFile = new File(repository + "\\" + projectName + "\\bom\\" + bomName + ".bom");
			File vocFile = new File(repository + "\\" + projectName + "\\bom\\" + bomName + "_en_US.voc");

			PrintStream psBOM = new PrintStream(bomFile);
			PrintStream psVocab = new PrintStream(vocFile);
			psBOM.println("property loadGetterSetterAsProperties \"true\"");
			psBOM.println("property origin \"xom:/" + projectName + "//" + projectName + "-xom\"");
			psBOM.println("property uuid \"" + UUID.randomUUID() + "\"");

			psVocab.println("# Vocabulary Properties");
			psVocab.println("uuid = " + UUID.randomUUID());

			HashMap<String, List<BOMClass>> packageMap = new HashMap<String, List<BOMClass>>();
			for (BOMClass classHolder : classHolderList) {
				String key = classHolder.getPackageName();
				List<BOMClass> packageClassHolderList = packageMap.get(key);
				if (packageClassHolderList == null) {
					packageClassHolderList = new ArrayList<BOMClass>();
				}
				packageClassHolderList.add(classHolder);
				packageMap.put(key, packageClassHolderList);
			}

			for (String packageName : packageMap.keySet()) {
				List<BOMClass> packageClassHolderList = packageMap.get(packageName);

				psBOM.println("package " + packageName + ";");
				psVocab.println("# " + packageName);

				for (BOMClass classHolder : packageClassHolderList) {

					psBOM.println("public class " + classHolder.getClassName() + " {");
					psBOM.println("public  " + classHolder.getClassName() + " ();");

					List<BOMElement> elements = classHolder.getBomElements();
					for (BOMElement element : elements) {
						// for (String modifier : element.getModifers()) {
						// psBOM.print(modifier + " ");
						// }
						if (element.getIsStatic().equals("Y")) {
							psBOM.print("static ");
						}
						if (element.getIsFinal().equals("Y")) {
							psBOM.print("final ");
						}
						psBOM.print(element.getDataType() + " ");
						String elementName = element.getElementName().trim();
						if (elementName.contains("(")) {
							int index1 = 0;
							int index2 = elementName.indexOf(' ');
							StringBuffer vocabElementName = new StringBuffer();
							while (index2 >= 0 && index2 < elementName.length()) {
								vocabElementName.append(elementName.substring(index1, index2));
								int index3 = elementName.indexOf(',', index2);
								if (index3 == -1) {
									index3 = elementName.indexOf(')', index2);
								}
								vocabElementName.append(elementName.charAt(index3));
								index1 = index3 + 1;
								if (index3 > 0) {
									index2 = elementName.indexOf(' ', index3 + 1);
								} else {
									index2++;
								}

							}
							psBOM.print(vocabElementName.toString());
							System.out.println("vocabElementName : " + vocabElementName);
						} else {
							psBOM.print(elementName);
						}
						psBOM.println(";");
						if (!element.getIsFinal().equals("Y")) {

							psVocab.println(
									packageName + "." + classHolder.getClassName() + "." + element.getElementName()
											+ "#phrase.action = set the " + element.getVerbalization());
							if (!element.getIsStatic().equals("Y")) {
								psVocab.println(" of {this} ");
							}
							psVocab.println(" to {" + element.getVerbalization() + "}");
						}
						psVocab.println(packageName + "." + classHolder.getClassName() + "." + element.getElementName()
								+ "#phrase.navigation = ");
						if (!element.getIsStatic().equals("Y")) {
							psVocab.println("{");
						}

						psVocab.println(element.getVerbalization());
						if (!element.getIsStatic().equals("Y")) {
							psVocab.println("} of {this}");
						}
					}
					psBOM.println("}");
				}

			}

			psBOM.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	//
	// public List<RuleDefinition> extractDefinitions(BusinessRule businessRule)
	// {
	// List<RuleDefinition> ruleDefinitionList=new ArrayList<RuleDefinition>();
	// List<BusinessRuleStatement> businessRuleStatementList=
	// businessRule.getBusinessRuleStatements();
	// for (BusinessRuleStatement businessRuleStatement :
	// businessRuleStatementList) {
	// businessRuleStatement.
	// }
	//
	// return ruleDefinitionList;
	//
	// }

	public void createBRL(String repository, String projectName, List<BusinessRule> businessRules) {

		try {
			File dir = new File(repository + "\\" + projectName);
			dir.mkdirs();

			for (BusinessRule businessRule : businessRules) {
				File packageDir = new File(repository + "\\" + projectName + "\\rules\\"
						+ businessRule.getRulePackage().replace('.', '\\'));
				packageDir.mkdirs();
				File brlFile = new File(
						packageDir.getAbsolutePath() + "\\" + businessRule.getRuleName().trim() + ".brl");

				PrintStream psBRL = new PrintStream(brlFile);
				psBRL.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

				psBRL.println(
						"<ilog.rules.studio.model.brl:ActionRule xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:ilog.rules.studio.model.brl=\"http://ilog.rules.studio/model/brl.ecore\">");
				psBRL.println("<name>" + businessRule.getRuleName() + "</name>");
				psBRL.println("<uuid>" + UUID.randomUUID() + "</uuid>");
				psBRL.println("<documentation><![CDATA[" + businessRule.getRuleDocumentation() + "]]></documentation>");

				if (businessRule.getPriority() != null && businessRule.getPriority().trim().length() > 0) {
					psBRL.println("<priority>" + businessRule.getPriority() + "</priority>");
				}
				psBRL.println("<locale>en_US</locale>");
				psBRL.println("<definition><![CDATA[");
				if (businessRule.getRuleDefinitions() != null && businessRule.getRuleDefinitions().size() > 0) {

					psBRL.println("definitions");
					for (RuleDefinition definition : businessRule.getRuleDefinitions()) {
						// RuleDefinition
						// definition=businessRule.getRuleDefinitions();
						char ch = ' ';
						if (definition.getDefinitionType() != null && definition.getDefinitionType().length() > 0)
							ch = definition.getDefinitionType().charAt(0);
						String article = "a";
						if (isVowel(ch)) {
							article = "an";
						}
						psBRL.print("\tset 'the " + definition.getTerm() + "' to " + article + " "
								+ definition.getDefinitionType());
						String referenceElementTerm = definition.getReferenceElementTerm();
						String collectionElementTerm = definition.getCollectionReferenceTerm();
						if (null != collectionElementTerm && collectionElementTerm.trim().length() > 0) {
							psBRL.print(" in  " + collectionElementTerm + " ");
						} else if (null != referenceElementTerm && referenceElementTerm.trim().length() > 0) {
							psBRL.print(" from  " + referenceElementTerm + " ");
						}

						String referenceHolderTerm = definition.getReferenceHolderTerm();
						if (null != referenceHolderTerm && referenceHolderTerm.trim().length() > 0) {
							psBRL.print(" of " + referenceHolderTerm + " ");
						}
						String filter = definition.getFilter();

						if (null != filter && filter.trim().length() > 0) {
							psBRL.println();
							psBRL.print("\t\twhere " + filter);

						}
						psBRL.println(";");

					}
				}
				if (businessRule.getRuleConditions() != null // ) {
						&& businessRule.getRuleConditions().size() > 0) {
					psBRL.println("if ");
					int andCount = 0;
					int orCount = 0;
					for (int i = 0; i < businessRule.getRuleConditions().size(); i++) {
						String operator = businessRule.getRuleConditions().get(i).getConcatinationOperator();
						if (operator.equalsIgnoreCase("AND")) {
							andCount++;
						} else if (operator.equalsIgnoreCase("OR")) {
							orCount++;
						}
					}
					int n = businessRule.getRuleConditions().size();
					boolean andOrConditionOnly = false;
					if (n > 1 && orCount >= n - 1 && andCount == 0) {
						psBRL.println("\tany of the following conditions is true:");
						andOrConditionOnly = true;
					} else if (n > 1 && andCount >= n - 1 && orCount == 0) {
						psBRL.println("\tall of the following conditions are true:");
						andOrConditionOnly = true;
					}
					// psBRL.println("all of the following conditions are true:");
					for (RuleCondition condition : businessRule.getRuleConditions()) {

						// RuleCondition condition =
						// businessRule.getRuleConditions();

						psBRL.print("\t\t");
						if (andOrConditionOnly) {
							psBRL.print(" - ");
						}

						psBRL.print(condition.getTerm() + " ");
						String referenceTerm = condition.getReferenceTerm();
						if (null != referenceTerm && referenceTerm.trim().length() > 0) {
							psBRL.print("of 'the " + referenceTerm + "' ");
						}
						String conditionalOperator = condition.getConditionalOperator();
						if (null != conditionalOperator) {
							psBRL.print(conditionalOperator + " ");
						}
						String value = condition.getValue();
						if (null != value) {
							psBRL.print(value + " ");
						}
						String concatinationOperator = condition.getConcatinationOperator();
						if (null != concatinationOperator && !andOrConditionOnly) {
							psBRL.print(concatinationOperator + " ");
						}
						psBRL.println();

					}
				}
				if (businessRule.getRuleActions() != null && businessRule.getRuleActions().size() > 0) {

					psBRL.println("then");
					for (RuleAction action : businessRule.getRuleActions()) {
						// RuleAction action = businessRule.getRuleActions();
						psBRL.print("\t");
						String value = action.getValue();
						if (null != value && value.trim().length() > 0) {
							psBRL.print("set ");
						}
						psBRL.print(action.getTerm() + " ");
						String referenceTerm = action.getReferenceTerm();
						if (null != referenceTerm && referenceTerm.trim().length() > 0) {
							psBRL.print("of 'the " + referenceTerm + "' ");
						}

						if (null != value && value.trim().length() > 0) {
							psBRL.print("to " + value + " ");
						}

						psBRL.println(";");
					}
				}

				psBRL.println("]]></definition>");
				psBRL.println("</ilog.rules.studio.model.brl:ActionRule>");
				psBRL.close();

				System.out.println(businessRule.getRulePackage() + "." + businessRule.getRuleName() + " -  generated");
			}

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private boolean isVowel(char ch) {

		if (ch == 'a' || ch == 'A' || ch == 'e' || ch == 'E' || ch == 'i' || ch == 'I' || ch == 'o' || ch == 'O'
				|| ch == 'u' || ch == 'U') {
			return true;
		} else {
			return false;
		}

	}

	public void createDecisionTable(RuleProject ruleProject, DecisionTable decisionTable) {

		String repository = ruleProject.getRepositoryLocation();
		String projectName = ruleProject.getRuleProjectName();
		File packageDir = new File(
				repository + "\\" + projectName + "\\rules\\" + decisionTable.getRulePackage().replace('.', '\\'));
		packageDir.mkdirs();
		File dtaFile = new File(packageDir.getAbsolutePath() + "\\" + decisionTable.getDecisionTableName() + ".dta");

		PrintStream psDTA;
		try {
			psDTA = new PrintStream(dtaFile);
			psDTA.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

			psDTA.println(
					"<ilog.rules.studio.model.dt:DecisionTable xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:ilog.rules.studio.model.dt=\"http://ilog.rules.studio/model/dt.ecore\">");
			psDTA.println("<name>" + decisionTable.getDecisionTableName() + "</name>");
			psDTA.println("<uuid>" + UUID.randomUUID() + "</uuid>");
			psDTA.println("<documentation><![CDATA[" + decisionTable.getDocumentation() + "]]></documentation>");
			psDTA.println("<locale>en_US</locale>");
			psDTA.println("<definition>");
			psDTA.println("<DT Version=\"7.0\" xmlns=\"http://schemas.ilog.com/Rules/7.0/DecisionTable\">");
			psDTA.println("<Body>");

			List<RuleDefinition> ruleDefinitions = decisionTable.getRuleDefinitions();
			List<RuleCondition> ruleConditions = decisionTable.getRuleConditions();
			if (ruleDefinitions.size() > 0 || ruleConditions.size() > 0) {
				psDTA.println("<Preconditions>");

				psDTA.print("<Text><![CDATA[");

				// ///

				if (ruleDefinitions.size() > 0) {
					psDTA.println("definitions");
					for (RuleDefinition definition : ruleDefinitions) {
						char ch = definition.getDefinitionType().charAt(0);
						String article = "a";
						if (isVowel(ch)) {
							article = "an";
						}
						psDTA.print("\tset 'the " + definition.getTerm() + "' to " + article + " "
								+ definition.getDefinitionType());
						String referenceElementTerm = definition.getReferenceElementTerm();
						String collectionElementTerm = definition.getCollectionReferenceTerm();
						if (null != collectionElementTerm && collectionElementTerm.trim().length() > 0) {
							psDTA.print(" in  " + collectionElementTerm + " ");
						} else if (null != referenceElementTerm && referenceElementTerm.trim().length() > 0) {
							psDTA.print(" from  " + referenceElementTerm + " ");
						}

						String referenceHolderTerm = definition.getReferenceHolderTerm();
						if (null != referenceHolderTerm && referenceHolderTerm.trim().length() > 0) {
							psDTA.print(" of " + referenceHolderTerm + " ");
						}
						String filter = definition.getFilter();

						if (null != filter && filter.trim().length() > 0) {
							psDTA.println();
							psDTA.print("\t\twhere " + filter);

						}
						psDTA.println(";");

					}
				}
				if (ruleConditions.size() > 0) {
					psDTA.println("if ");
					// psBRL.println("all of the following conditions are true:");
					for (RuleCondition condition : ruleConditions) {
						psDTA.print("\t");
						psDTA.print(condition.getTerm() + " ");
						String referenceTerm = condition.getReferenceTerm();
						if (null != referenceTerm && referenceTerm.trim().length() > 0) {
							psDTA.print("of 'the " + referenceTerm + "' ");
						}
						String conditionalOperator = condition.getConditionalOperator();
						if (null != conditionalOperator) {
							psDTA.print(conditionalOperator + " ");
						}
						String value = condition.getValue();
						if (null != value) {
							psDTA.print(value + " ");
						}
						String concatinationOperator = condition.getConcatinationOperator();
						if (null != concatinationOperator) {
							psDTA.print(concatinationOperator + " ");
						}
						psDTA.println();

					}
				}

				// ///
				// if (ruleDefinitions.size() > 0) {
				// psDTA.println("definitions");
				// }
				//
				// for (RuleDefinition ruleDefinition : ruleDefinitions) {
				// psDTA.println("set 'the " + ruleDefinition.getTerm()
				// + "' to a " + ruleDefinition.getTerm() + ";");
				// }
				// if (ruleConditions.size() > 0) {
				// psDTA.println("if");
				// }
				// for (RuleCondition ruleCondition : ruleConditions) {
				// psDTA.print("\t");
				// psDTA.print(ruleCondition.getTerm() + " ");
				// String referenceTerm = ruleCondition.getReferenceTerm();
				// if (null != referenceTerm
				// && referenceTerm.trim().length() > 0) {
				// psDTA.print("of 'the " + referenceTerm + "' ");
				// }
				// String conditionalOperator = ruleCondition
				// .getConditionalOperator();
				// if (null != conditionalOperator) {
				// psDTA.print(conditionalOperator + " ");
				// }
				// String value = ruleCondition.getValue();
				// if (null != value) {
				// psDTA.print(value + " ");
				// }
				// String concatinationOperator = ruleCondition
				// .getConcatinationOperator();
				// if (null != concatinationOperator) {
				// psDTA.print(concatinationOperator + " ");
				// }
				// psDTA.println();
				// }

				psDTA.println("]]></Text>");

				psDTA.println("</Preconditions>");
			}

			psDTA.println("<Structure>");

			List<ConditionColumn> conditionColumns = decisionTable.getConditionColumns();
			psDTA.println("<ConditionDefinitions>");

			for (int i = 0; i < conditionColumns.size(); i++) {
				ConditionColumn decisionTableColumn = conditionColumns.get(i);
				psDTA.println("\t<ConditionDefinition Id=\"C" + i + "\">");
				psDTA.println("\t\t <ExpressionDefinition>");
				psDTA.print("\t\t\t <Text><![CDATA[");
				psDTA.print(decisionTableColumn.getColumnTerm());
				// + " is <a number>");
				psDTA.println("]]></Text>");
				psDTA.println("\t\t </ExpressionDefinition>");
				psDTA.println("\t</ConditionDefinition>");

			}
			psDTA.println("</ConditionDefinitions>");
			psDTA.println("<ActionDefinitions>");
			List<ActionColumn> actionColumns = decisionTable.getActionColumns();
			for (int i = 0; i < actionColumns.size(); i++) {
				ActionColumn decisionTableColumn = actionColumns.get(i);
				psDTA.println("\t<ActionDefinition Id=\"A" + i + "\">");
				psDTA.println("\t\t <ExpressionDefinition>");
				psDTA.print("\t\t\t <Text><![CDATA[");
				psDTA.print(decisionTableColumn.getColumnTerm());
				// + " to <a number>");
				psDTA.println("]]></Text>");
				psDTA.println("\t\t </ExpressionDefinition>");
				psDTA.println("\t</ActionDefinition>");

			}
			psDTA.println("</ActionDefinitions>");

			psDTA.println("</Structure>");
			psDTA.println("<Contents>");
			ArrayList<ArrayList<NameValuePair>> conditionData = decisionTable.getConditionData();
			ArrayList<ArrayList<NameValuePair>> actionData = decisionTable.getActionData();

			if (conditionData.size() > 0) {
				int numOfRows = conditionData.get(0).size();
				for (int rowNum = 0; rowNum < numOfRows; rowNum++) {
					int conditionTags = 0;

					for (int i = 0, j = 0; i < conditionData.size(); i++) {
						ArrayList<NameValuePair> conditionColumnData = conditionData.get(i);

						NameValuePair cellData = conditionColumnData.get(rowNum);
						if (cellData.getValue() != null && cellData.getValue().trim().length() > 0) {

							// if (i == 0 || (i != 0 && rowNum != 0))
							if (rowNum == 0 || (rowNum != 0 && i != 0)) {
								psDTA.println("<Partition DefId=\"C" + i + "\">");
							}
							j++;
							psDTA.println("<Condition>");
							psDTA.println("<Expression>");

							if (cellData.getValue().equals("\"@OTHERWISE\""))
								psDTA.println("<Otherwise/>");
							else {
								psDTA.print("<Param>");
								psDTA.print("<![CDATA[");
								psDTA.print(cellData.getValue());
								psDTA.print("]]>");
								psDTA.print("</Param>");
							}
							psDTA.println("</Expression>");
							conditionTags++;
						} else if (j == 0) {
							psDTA.println("<Condition>");
							psDTA.println("<Expression/>");
							j++;
							conditionTags++;
						}

					}

					psDTA.println("<ActionSet>");
					for (int i = 0; i < actionData.size(); i++) {
						ArrayList<NameValuePair> actionColumnData = actionData.get(i);

						if (rowNum < actionColumnData.size()) {
							NameValuePair cellData = actionColumnData.get(rowNum);
							if (cellData.getValue() != null) {
								if (cellData.getValue().equals("\"@IGNORE\"")) {
									psDTA.println("<Action DefId=\"A" + i + "\" Enabled=\"false\">");
									psDTA.println("<Expression>");
									psDTA.println(" <Param><![CDATA[]]></Param>");
									psDTA.println("</Expression>");
									psDTA.println("</Action>");
								} else {
									psDTA.println("<Action DefId=\"A" + i + "\">");
									psDTA.println("<Expression>");
									psDTA.println(" <Param><![CDATA[" + cellData.getValue() + "]]></Param>");
									psDTA.println("</Expression>");
									psDTA.println("</Action>");
								}
							}
						}
					}
					psDTA.println("</ActionSet>");
					// for(int columnNum=0;columnNum<
					// conditionColumns.size();columnNum++) {
					for (int i = 0; i < conditionTags; i++) {

						psDTA.println("</Condition>");
						if (i != conditionTags - 1) {
							psDTA.println("</Partition>");
						}
					}
					// }

					// for(int columnNum=0;columnNum<
					// actionColumns.size();columnNum++) {
					// ArrayList<NameValuePair> actionRowData =
					// actionData.get(i);
					// psDTA.println("<ActionSet>");
					// for (int j = 0; j < actionRowData.size(); j++) {
					// NameValuePair cellData = actionRowData.get(j);
					// if (cellData.getValue() != null) {
					//
					// psDTA.println("<Action DefId=\"A" + j + "\">");
					// psDTA.println("<Expression>");
					// psDTA.println(" <Param><![CDATA[" + cellData.getValue()
					// + "]]></Param>");
					// psDTA.println("</Expression>");
					// psDTA.println("</Action>");
					// }
					// }

				}
			}
			// for(int columnNum=0;columnNum<
			// conditionColumns.size();columnNum++) {
			// for (int i = 0; i < conditionData.size(); i++) {
			// ArrayList<NameValuePair> conditionRowData = conditionData
			// .get(i);
			// // for (int j = 0; j < conditionRowData.size(); j++) {
			// NameValuePair cellData = conditionRowData.get(columnNum);
			// if (cellData.getValue() != null) {
			// if (i == 0 || (i != 0 && columnNum != 0)) {
			// psDTA.println("<Partition DefId=\"C" + columnNum + "\">");
			// }
			// psDTA.println("<Condition>");
			// psDTA.println("<Expression>");
			//
			// psDTA.print("<Param>");
			// psDTA.print("<![CDATA[");
			// psDTA.print(cellData.getValue());
			// psDTA.print("]]>");
			// psDTA.print("</Param>");
			// psDTA.println("</Expression>");
			// }
			// }
			// }

			// for(int columnNum=0;columnNum< actionColumns.size();columnNum++)
			// {
			// ArrayList<NameValuePair> actionRowData = actionData.get(i);
			// psDTA.println("<ActionSet>");
			// for (int j = 0; j < actionRowData.size(); j++) {
			// NameValuePair cellData = actionRowData.get(j);
			// if (cellData.getValue() != null) {
			//
			// psDTA.println("<Action DefId=\"A" + j + "\">");
			// psDTA.println("<Expression>");
			// psDTA.println(" <Param><![CDATA[" + cellData.getValue()
			// + "]]></Param>");
			// psDTA.println("</Expression>");
			// psDTA.println("</Action>");
			// }
			// }
			// psDTA.println("</ActionSet>");
			// for(int columnNum=0;columnNum<
			// conditionColumns.size();columnNum++) {
			// for (int i = 0; i < conditionData.size(); i++) {
			// ArrayList<NameValuePair> conditionRowData = conditionData
			// .get(i);
			// // for (int j = 0; j < conditionRowData.size(); j++) {
			// NameValuePair cellData = conditionRowData.get(columnNum);
			// if (cellData.getValue() != null) {
			// psDTA.println("</Condition>");
			// if (j != conditionRowData.size() - 1) {
			// psDTA.println("</Partition>");
			// }
			// }
			// }
			//
			// }

			psDTA.println("</Partition>");
			psDTA.println("</Contents>");

			psDTA.println("</Body>");

			psDTA.println("<Resources DefaultLocale=\"en_US\">");
			psDTA.println("<ResourceSet Locale=\"en_US\">");

			for (int i = 0; i < actionColumns.size(); i++) {
				ActionColumn decisionTableColumn = actionColumns.get(i);
				psDTA.println("<Data Name=\"Definitions(A" + i + ")#HeaderText\"><![CDATA["
						+ decisionTableColumn.getColumnName() + "]]></Data>");
			}

			for (int i = 0; i < conditionColumns.size(); i++) {
				ConditionColumn decisionTableColumn = conditionColumns.get(i);
				psDTA.println("<Data Name=\"Definitions(C" + i + ")#HeaderText\"><![CDATA["
						+ decisionTableColumn.getColumnName() + "]]></Data>");
			}

			// psDTA.println("<Data Name=\"Definitions(A0)#HeaderText\"><![CDATA[Element
			// One]]></Data>");
			//
			// psDTA.println("<Data Name=\"Definitions(C0)#HeaderText\"><![CDATA[Element
			// One]]></Data>");
			//
			// psDTA.println("<Data Name=\"Definitions(C1)#HeaderText\"><![CDATA[Element
			// Two]]></Data>");

			psDTA.println("</ResourceSet>");
			psDTA.println("</Resources>");
			psDTA.println("</DT></definition>");
			psDTA.println("</ilog.rules.studio.model.dt:DecisionTable>");
			psDTA.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public void createProject(RuleProject ruleProject, String[] dependentProjects) {
		try {
			String repository = ruleProject.getRepositoryLocation();
			String projectName = ruleProject.getRuleProjectName();
			File dir = new File(repository + "\\" + projectName);
			dir.mkdirs();
			File projectFile = new File(repository + "\\" + projectName + "\\" + ".project");

			PrintStream ps = new PrintStream(projectFile);
			ps.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
			ps.println("<projectDescription>");
			ps.print("<name>");
			ps.print(projectName);
			ps.println("</name>");
			ps.println("<comment></comment>");
			ps.println("<projects>");

			for (String dependentProject : dependentProjects) {
				ps.println("<project>" + dependentProject + "</project>");
			}
			ps.println("</projects>");
			ps.println("<buildSpec>");
			ps.println("<buildCommand>");
			ps.println("<name>ilog.rules.studio.model.ruleBuilder</name>");
			ps.println("<arguments>");
			ps.println("</arguments>");
			ps.println("</buildCommand>");
			ps.println("</buildSpec>");
			ps.println("<natures>");
			ps.println("<nature>ilog.rules.studio.model.decisionProject</nature>");
			ps.println("<nature>ilog.rules.studio.model.operationProject</nature>");
			ps.println("<nature>ilog.rules.studio.model.ruleNature</nature>");
			ps.println("</natures>");
			ps.println("</projectDescription>");
			ps.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public void createRuleProject(RuleProject ruleProject) {
		String repository = ruleProject.getRepositoryLocation();
		String projectName = ruleProject.getRuleProjectName();
		File dir = new File(repository + "\\" + projectName);
		dir.mkdirs();

		File ruleDir = new File(repository + "\\" + projectName + "\\rules");
		ruleDir.mkdirs();
		File bomDir = new File(repository + "\\" + projectName + "\\bom");
		bomDir.mkdirs();
		File templatesDir = new File(repository + "\\" + projectName + "\\templates");
		templatesDir.mkdirs();
		File queriesDir = new File(repository + "\\" + projectName + "\\queries");
		queriesDir.mkdirs();

		File projectFile = new File(repository + "\\" + projectName + "\\" + ".ruleproject");

		PrintStream ps;
		try {
			ps = new PrintStream(projectFile);
			ps.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
			ps.println(
					"<ilog.rules.studio.model.base:RuleProject xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:com.ibm.rules.studio.model.decisionservice=\"http://com.ibm.rules.studio/model/decisionservice.ecore\" xmlns:ilog.rules.studio.model.base=\"http://ilog.rules.studio/model/base.ecore\" xmlns:ilog.rules.studio.model.bom=\"http://ilog.rules.studio/model/bom.ecore\" xmlns:ilog.rules.studio.model.query=\"http://ilog.rules.studio/model/query.ecore\" xmlns:ilog.rules.studio.model.rule=\"http://ilog.rules.studio/model/rule.ecore\" xmlns:ilog.rules.studio.model.xom=\"http://ilog.rules.studio/model/xom.ecore\" buildMode=\"DecisionEngine\" isADecisionService=\"true\" migrationFlag=\"3\">");
			ps.print("<name>");
			ps.print(projectName);
			ps.println("</name>");
			ps.println("<uuid>" + UUID.randomUUID() + "</uuid>");
			ps.println("<outputLocation>output</outputLocation>");
			ps.println("<categories>any</categories>");

			ps.println("<paths xsi:type=\"ilog.rules.studio.model.xom:XOMPath\" pathID=\"XOM\">");
			ps.println(
					"<entries xsi:type=\"ilog.rules.studio.model.xom:LibraryXOMPathEntry\" name=\"org.eclipse.jdt.launching.JRE_CONTAINER\" url=\"file:org.eclipse.jdt.launching.JRE_CONTAINER\" kind=\"LIBRARY\" exported=\"false\"/>");
			ps.println("<entries xsi:type=\"ilog.rules.studio.model.xom:SystemXOMPathEntry\" name=\""
					+ ruleProject.getRuleProjectName() + "-xom\" url=\"platform:/" + ruleProject.getRuleProjectName()
					+ "-xom\" kind=\"JAVA_PROJECT\" exported=\"true\"/>");
			ps.println("</paths>");
			ps.println("<paths xsi:type=\"ilog.rules.studio.model.bom:BOMPath\" pathID=\"BOM\">");
			ps.println("<entries xsi:type=\"ilog.rules.studio.model.bom:BOMEntry\" name=\"model\" url=\"platform:/"
					+ ruleProject.getRuleProjectName() + "/bom/model.bom\" origin=\"xom:/"
					+ ruleProject.getRuleProjectName() + "//" + ruleProject.getRuleProjectName() + "-xom\"/>");
			ps.println("</paths>");

//			List<RuleSetParameter> ruleSetParameterList = ruleProject
//					.getRuleSetParameters();
//
//			if (ruleSetParameterList != null) {
//				for (RuleSetParameter ruleSetParameter : ruleSetParameterList) {
//					ps.println("<parameters name=\""
//							+ ruleSetParameter.getName() + "\" type=\""
//							+ ruleSetParameter.getDataType()
//							+ "\" initialValue=\""
//							+ ruleSetParameter.getInitialValue()
//							+ "\" verbalization=\""
//							+ ruleSetParameter.getVerbalization()
//							+ "\" direction=\""
//							+ ruleSetParameter.getDirection() + "\"/>");
//				}
//			}

			ps.println(" <modelFolders xsi:type=\"ilog.rules.studio.model.base:SourceFolder\">");
			ps.println("		    <name>rules</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.bom:BOMFolder\">");
			ps.println("<name>bom</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.rule:TemplateFolder\">");
			ps.println("<name>templates</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.query:QueryFolder\">");
			ps.println("<name>queries</name>");
			ps.println("</modelFolders>");

			ps.println("<modelFolders xsi:type=\"ilog.rules.studio.model.base:ResourceFolder\">");
			ps.println("<name>resources</name>");
			ps.println("</modelFolders>");
			ps.println("<modelFolders xsi:type=\"com.ibm.rules.studio.model.decisionservice:OperationFolder\">");
			ps.println("<name>deployment</name>");
			ps.println("</modelFolders>");

			ps.println("</ilog.rules.studio.model.base:RuleProject>");
			ps.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public void createBOM(RuleProject ruleProject, BOM bom) {
		// TODO Auto-generated method stub
		String repository = ruleProject.getRepositoryLocation();
		String projectName = ruleProject.getRuleProjectName();
		try {
			File dir = new File(repository + "\\" + projectName);
			dir.mkdirs();
			String bomName = bom.getBomName();
			File bomFile = new File(repository + "\\" + projectName + "\\bom\\" + bomName.trim() + ".bom");
			File vocFile = new File(repository + "\\" + projectName + "\\bom\\" + bomName.trim() + "_en_US.voc");

			PrintStream psBOM = new PrintStream(bomFile);
			PrintStream psVocab = new PrintStream(vocFile);
			psBOM.println("property loadGetterSetterAsProperties \"true\"");
			psBOM.println("property origin \"xom:/" + projectName + "//" + projectName + "-xom\"");

			psBOM.println("property uuid \"" + UUID.randomUUID() + "\"");

			psVocab.println("# Vocabulary Properties");
			psVocab.println("uuid = " + UUID.randomUUID());

			HashMap<String, List<BOMClass>> packageMap = new HashMap<String, List<BOMClass>>();
			List<BOMClass> bomClasses = bom.getBomClasses();
			for (BOMClass classHolder : bomClasses) {
				String key = classHolder.getPackageName();
				List<BOMClass> packageClassHolderList = packageMap.get(key);
				if (packageClassHolderList == null) {
					packageClassHolderList = new ArrayList<BOMClass>();
				}
				packageClassHolderList.add(classHolder);
				packageMap.put(key, packageClassHolderList);
			}

			for (String packageName : packageMap.keySet()) {
				List<BOMClass> packageClassHolderList = packageMap.get(packageName);

				psBOM.println("package " + packageName + ";");

				for (BOMClass classHolder : packageClassHolderList) {
					psVocab.println("# " + packageName + "." + classHolder.getClassName());
					if (classHolder.getVerbalization() != null) {
						String verbalization = classHolder.getVerbalization().trim();
						if (verbalization.length() > 0) {
							psVocab.println(packageName + "." + classHolder.getClassName() + "#concept.label = "
									+ verbalization);
						}
					}
//					psVocab.println("# " + packageName + "."
//							+ classHolder.getClassName());
					psBOM.println("public class " + classHolder.getClassName() + " {");
					psBOM.println("public  " + classHolder.getClassName() + " ();");

					List<BOMElement> elements = classHolder.getBomElements();
					for (BOMElement element : elements) {
						// for (String modifier : element.getModifers()) {
						// psBOM.print(modifier + " ");
						// }
						if (element.getIsStatic().equals("Y")) {
							psBOM.print("static ");
						}
						if (element.getIsFinal().equals("Y")) {
							psBOM.print("final ");
						}
						psBOM.print(element.getDataType() + " ");
						psBOM.print(element.getElementName());
						psBOM.println(";");

						String elementName = element.getElementName();
						if (elementName != null)
							elementName = elementName.trim();
						StringBuffer vocabElementName = new StringBuffer();
						if (Objects.requireNonNull(elementName).contains("(")) {
							int index1 = 0;
							int index2 = elementName.indexOf(' ');

							if (index2 == -1) {
								vocabElementName = new StringBuffer(elementName);
							}

							while (index2 >= 0 && index2 < elementName.length()) {
								vocabElementName.append(elementName.substring(index1, index2));
								int index3 = elementName.indexOf(',', index2);
								if (index3 == -1) {
									index3 = elementName.indexOf(')', index2);
								}
								vocabElementName.append(elementName.charAt(index3));
								index1 = index3 + 1;
								if (index3 > 0) {
									index2 = elementName.indexOf(' ', index3 + 1);
								} else {
									index2++;
								}

							}

						} else {
							vocabElementName = new StringBuffer(elementName);
						}
						if (!element.getIsFinal().equals("Y")) {

							psVocab.print(
									packageName + "." + classHolder.getClassName() + "." + vocabElementName.toString()
											+ "#phrase.action = set the " + element.getVerbalization());
							if (!element.getIsStatic().equals("Y")) {
								psVocab.print(" of {this} ");
							}
							psVocab.print(" to {" + element.getVerbalization() + "}");

							psVocab.println();
						} else if (element.getDataType().equalsIgnoreCase("VOID")) {
							psVocab.print(packageName + "." + classHolder.getClassName() + "."
									+ vocabElementName.toString() + "#phrase.action = " + element.getVerbalization());
							if (!element.getIsStatic().equals("Y")) {
								psVocab.print(" of {this} ");
							}

							psVocab.println();
						}
						if (!element.getDataType().equalsIgnoreCase("VOID")) {
							psVocab.print(packageName + "." + classHolder.getClassName() + "."
									+ vocabElementName.toString() + "#phrase.navigation = ");
							if (!element.getIsStatic().equals("Y")) {
								psVocab.print("{");
							}

							psVocab.print(element.getVerbalization());
							if (!element.getIsStatic().equals("Y")) {
								psVocab.print("} of {this}");
							}
						}
						psVocab.println();
					}
					psBOM.println("}");

				}

			}

			psBOM.close();
			psVocab.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void createBRL(RuleProject ruleProject, BusinessRule businessRule) throws FileNotFoundException {

		File packageDir = new File(ruleProject.repositoryLocation + "\\" + ruleProject.ruleProjectName + "\\rules\\"
				+ businessRule.getRulePackage().replace('.', '\\'));
		packageDir.mkdirs();
		File brlFile = new File(packageDir.getAbsolutePath() + "\\" + businessRule.getRuleName().trim() + ".brl");

		PrintStream psBRL = new PrintStream(brlFile);
		psBRL.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

		psBRL.println(
				"<ilog.rules.studio.model.brl:ActionRule xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:ilog.rules.studio.model.brl=\"http://ilog.rules.studio/model/brl.ecore\">");
		psBRL.println("<name>" + businessRule.getRuleName() + "</name>");
		psBRL.println("<uuid>" + UUID.randomUUID() + "</uuid>");
		psBRL.println("<documentation><![CDATA[" + businessRule.getRuleDocumentation() + "]]></documentation>");

		if (businessRule.getPriority() != null && businessRule.getPriority().trim().length() > 0) {
			psBRL.println("<priority>" + businessRule.getPriority() + "</priority>");
		}
		psBRL.println("<locale>en_US</locale>");
		psBRL.println("<definition><![CDATA[");
		if (businessRule.getRuleDefinitions() != null && businessRule.getRuleDefinitions().size() > 0) {

			psBRL.println("definitions");
			for (RuleDefinition definition : businessRule.getRuleDefinitions()) {
				// RuleDefinition
				// definition=businessRule.getRuleDefinitions();
				char ch = ' ';

				// a from restriction
				boolean isDefitionTypeFound = false;

				String article = "a";
				if (definition.getDefinitionType() != null && definition.getDefinitionType().length() > 0) {
					isDefitionTypeFound = true;
					ch = definition.getDefinitionType().charAt(0);
					if (isVowel(ch)) {
						article = "an";
					}
				}
				psBRL.print("\tset 'the " + definition.getTerm() + "' to ");

				if (isDefitionTypeFound)
					psBRL.print(article + " " + definition.getDefinitionType());

				String referenceElementTerm = definition.getReferenceElementTerm();
				String collectionElementTerm = definition.getCollectionReferenceTerm();
				if (null != collectionElementTerm && collectionElementTerm.trim().length() > 0 && isDefitionTypeFound)
					psBRL.print(" in  " + collectionElementTerm + " ");
				else if (null != referenceElementTerm && referenceElementTerm.trim().length() > 0
						&& isDefitionTypeFound)
					psBRL.print(" from  " + referenceElementTerm + " ");
				else
					psBRL.print(" " + referenceElementTerm);

				String referenceHolderTerm = definition.getReferenceHolderTerm();
				if (null != referenceHolderTerm && referenceHolderTerm.trim().length() > 0) {
					psBRL.print(" of " + referenceHolderTerm + " ");
				}
				String filter = definition.getFilter();

				if (null != filter && filter.trim().length() > 0) {
					psBRL.println();
					psBRL.print("\t\twhere " + filter);

				}
				psBRL.println(";");

			}
		}
		if (businessRule.getRuleConditions() != null // ) {
				&& businessRule.getRuleConditions().size() > 0) {
			psBRL.println("if ");
			int andCount = 0;
			int orCount = 0;
			for (int i = 0; i < businessRule.getRuleConditions().size(); i++) {
				String operator = businessRule.getRuleConditions().get(i).getConcatinationOperator();
				if (operator != null && operator.equalsIgnoreCase("AND")) {
					andCount++;
				} else if (operator != null && operator.equalsIgnoreCase("OR")) {
					orCount++;
				}
			}
			int n = businessRule.getRuleConditions().size();
			boolean andOrConditionOnly = false;
			if (n > 1 && orCount >= n - 1 && andCount == 0) {
				psBRL.println("\tany of the following conditions is true:");
				andOrConditionOnly = true;
			} else if (n > 1 && andCount >= n - 1 && orCount == 0) {
				psBRL.println("\tall of the following conditions are true:");
				andOrConditionOnly = true;
			}
			// psBRL.println("all of the following conditions are true:");
			for (RuleCondition condition : businessRule.getRuleConditions()) {

				// RuleCondition condition =
				// businessRule.getRuleConditions();

				psBRL.print("\t\t");
				if (andOrConditionOnly) {
					psBRL.print(" - ");
				}

				psBRL.print(condition.getTerm() + " ");
				String referenceTerm = condition.getReferenceTerm();
				if (null != referenceTerm && referenceTerm.trim().length() > 0) {
					psBRL.print("of 'the " + referenceTerm + "' ");
				}
				String conditionalOperator = condition.getConditionalOperator();
				if (null != conditionalOperator) {
					psBRL.print(conditionalOperator + " ");
				}
				String value = condition.getValue();
				if (null != value) {
					psBRL.print(value + " ");
				}
				String concatinationOperator = condition.getConcatinationOperator();
				if (null != concatinationOperator && !andOrConditionOnly) {
					psBRL.print(concatinationOperator + " ");
				}
				psBRL.println();

			}
		}
		if (businessRule.getRuleActions() != null && businessRule.getRuleActions().size() > 0) {

			psBRL.println("then");
			for (RuleAction action : businessRule.getRuleActions()) {
				// RuleAction action = businessRule.getRuleActions();
				psBRL.print("\t");

				String term = action.getTerm();
				String value = action.getValue();
				String referenceTerm = action.getReferenceTerm();

				boolean isTermAvail = false;
				if (null != term && term.trim().length() > 0)
					isTermAvail = true;

				if (null != value && value.trim().length() > 0 && isTermAvail) {
					psBRL.print("set ");
					psBRL.print(action.getTerm() + " ");
				}
				if (null != referenceTerm && referenceTerm.trim().length() > 0) {
					psBRL.print("of 'the " + referenceTerm + "' ");
				}

				if (null != value && value.trim().length() > 0 && isTermAvail) {
					psBRL.print("to " + value + " ");
				} else
					psBRL.print(value + " ");

				psBRL.println(";");
			}
		}

		psBRL.println("]]></definition>");
		psBRL.println("</ilog.rules.studio.model.brl:ActionRule>");
		psBRL.close();

		System.out.println(businessRule.getRulePackage() + "." + businessRule.getRuleName() + " -  generated");

	}

	public void createVariableSet(RuleProject ruleProject) {
		List<Variable> variableSet = ruleProject.getVariableSet();

		String repository = ruleProject.getRepositoryLocation();
		String projectName = ruleProject.getRuleProjectName();

		File packageDir = new File(repository + "\\" + projectName + "\\rules\\");
		packageDir.mkdirs();

		File variablesFile = new File(packageDir.getAbsolutePath() + "\\" + "variables.var");
		PrintStream ps;
		try {
			ps = new PrintStream(variablesFile);
			ps.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
			ps.println(
					"<ilog.rules.studio.model.base:VariableSet xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:ilog.rules.studio.model.base=\"http://ilog.rules.studio/model/base.ecore\">");
			ps.println("<name>variables</name>");
			ps.println("<uuid>" + UUID.randomUUID() + "</uuid>");
			for (Variable variable : variableSet) {
				ps.println("<variables name=\"" + variable.getName() + "\" type=\"" + variable.getDataType()
						+ "\" initialValue=\"" + variable.getInitialValue() + "\" verbalization=\""
						+ variable.getVerbalization() + "\"/>");
			}
			ps.println("</ilog.rules.studio.model.base:VariableSet>");

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}