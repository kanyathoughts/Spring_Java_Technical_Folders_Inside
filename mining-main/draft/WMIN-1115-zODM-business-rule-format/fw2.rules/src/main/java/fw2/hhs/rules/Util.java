package fw2.hhs.rules;
//import java.util.ArrayList;
//
public class Util {
//
//	
//	
//	public static void main(String args[]) {
//		RuleManager ruleManager=new RuleManager();
//		ruleManager.createProject("c:\\PoC", "TestProject", new String[] {});
//		ruleManager.createRuleProject("c:\\PoC", "TestProject", new String[] {});
//	
//		
//		BOMClass holder= new BOMClass();
//		holder.setPackageName("com.util.mypack");
//		
//		holder.setClassName("TestClass1");
//		BOMElement e1=new BOMElement();
//		String[] modifers=new String[]{"public"};
//		e1.setModifers(modifers);
//		e1.setDataType("java.lang.Integer");
//		e1.setElementName("elementOne");
//		e1.setVerbalization("element one");
//		BOMElement e2=new BOMElement();
//		modifers=new String[]{"public"};
//		e2.setModifers(modifers);
//		e2.setDataType("java.lang.Integer");
//		e2.setElementName("elementTwo");
//		e2.setVerbalization("element two");
//		
//		BOMElement e3=new BOMElement();
//		modifers=new String[]{"public"};
//		e3.setModifers(modifers);
//		e3.setDataType("java.lang.Integer");
//		e3.setElementName("elementThree");
//		e3.setVerbalization("element three");
//		
//		BOMElement[] elements= new BOMElement[]{e1,e2,e3};
//		holder.setBomElements(elements);
//		
//		BOMClass holder1= new BOMClass();
//		holder1.setPackageName("com.util.mypack1");
//		
//		holder1.setClassName("TestClass2");
//
//		holder1.setBomElements(elements);
//		
//		BOMClass[] classBean=new BOMClass[]{holder, holder1};
//			
//		
//		ruleManager.createBOM("c:\\PoC", "TestProject", "model",classBean);
//		String repository = "c:\\PoC";
//		String projectName= "TestProject";
//		RuleDefinition[] ruleBeans=new RuleDefinition[2];
//		ruleBeans[0]=new RuleDefinition();
//		ruleBeans[1]=new RuleDefinition();
//		
//		ruleBeans[0].setRulePackage("myPack");
//		ruleBeans[0].setRuleName("test rule 1");
//		ArrayList<String> definitions=new ArrayList<String>();
//		definitions.add("test class1");
//		definitions.add("test class2");
//		
//		ArrayList<String> conditions=new ArrayList<String>();
//		conditions.add("the element one of 'test class1' is 1");
//		conditions.add("the element two of 'test class2' is 2");
//		
//		
//		ArrayList<String> actions=new ArrayList<String>();
//		actions.add("set the element one of 'test class1' to 2");
//		actions.add("set the element two of 'test class2' to 1");
//		
//		ruleBeans[0].setDefinitions(definitions);
//		ruleBeans[0].setConditions(conditions);
//		ruleBeans[0].setActions(actions);
//		
//		
//		ruleBeans[1].setRulePackage("myPack1");
//		ruleBeans[1].setRuleName("test rule 2");
//		definitions=new ArrayList<String>();
//		definitions.add("test class1");
//		definitions.add("test class2");
//		
//		conditions=new ArrayList<String>();
//		conditions.add("the element one of 'test class1' is 1");
//		conditions.add("the element two of 'test class2' is 2");
//		
//		
//		actions=new ArrayList<String>();
//		actions.add("set the element one of 'test class1' to 2");
//		actions.add("set the element two of 'test class2' to 1");
//		
//		ruleBeans[1].setDefinitions(definitions);
//		ruleBeans[1].setConditions(conditions);
//		ruleBeans[1].setActions(actions);
//		
//		ruleManager.createBRL(repository, projectName, ruleBeans);
//	}
//
}
