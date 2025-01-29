package fw2.hhs.rules;

import java.util.ArrayList;

public class Tester {

	
	public Tester() {
		EclipseProject eclipseProject=new EclipseProject();
		eclipseProject.setRepositoryLocation("C:\\POC\\ruleRepository");
		eclipseProject.setProjectName("TestProject");
		RuleProject ruleProject=new RuleProject();
		ruleProject.setRepositoryLocation("C:\\POC\\ruleRepository");
		ruleProject.setRuleProjectName("TestProject");
		
		Variable variable1=new Variable();
		variable1.setDataType("int");
		variable1.setName("param1");
		Variable ruleSetParameter2=new Variable();
		ruleSetParameter2.setDataType("int");
		ruleSetParameter2.setName("param2");
		ArrayList<Variable> variableSet=new ArrayList<Variable>();
		ruleProject.setVariableSet(variableSet);
	}
}
