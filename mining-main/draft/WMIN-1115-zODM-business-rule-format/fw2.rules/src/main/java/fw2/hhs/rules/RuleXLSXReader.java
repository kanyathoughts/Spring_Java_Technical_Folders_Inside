package fw2.hhs.rules;

import java.io.IOException;

import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.xssf.eventusermodel.XSSFReader;

public class RuleXLSXReader extends XSSFReader {

	public RuleXLSXReader(OPCPackage pkg) throws IOException, OpenXML4JException {
		super(pkg);
		// TODO Auto-generated constructor stub
	}
//    public RuleXLSXReader(InputStream mapFileStream) throws IOException {
//        super(mapFileStream);
//    }
//
//    public <T> T createInstance(Class<T> type, List<NameValuePair> convertedValueRow) throws Exception {
//        InstanceFactory factory = getInstanceFactory(type);
//        return factory.createInstance(type, convertedValueRow);
//    }
//
//    public <T> List<T> createInstances(Class<T> type, List<ArrayList<NameValuePair>> convertedValueRows) throws Exception {
//        InstanceFactory factory = getInstanceFactory(type);
//        return factory.createInstances(type, convertedValueRows);
//    }
//
//    public RuleProject readRuleProject(String name) throws Exception {
//        RuleProject dummy = new RuleProject();
//        dummy.setRuleProjectName(name);
//        List<RuleProject> editors = readObjects( "RuleProject", RuleProject.class, dummy);
//        for (RuleProject editor : editors) {
//            populateRuleProject(editor);
//            return editor;
//        }
//        return null;
//    }
//
//    public List<RuleProject> readRuleProjects() throws Exception {
//        List<RuleProject> editors = readObjects("RuleProject", RuleProject.class);
//        for (RuleProject editor : editors) {
//            populateRuleProject(editor);
//        }
//        return editors;
//    }
//
//    private <T> InstanceFactory getInstanceFactory(Class<T> type) {
//        return new Factory<T>();
//    }
//
//    private void populateRuleProject(RuleProject editor) throws Exception {
//        List<RuleSetParameter> ruleSetParameters = readObjects( "RuleSetParameter", RuleSetParameter.class, editor);
//        editor.setRuleSetParameters(ruleSetParameters);
//        List<BOM> boms = readObjects( "BOM", BOM.class, editor);
//        List<BusinessRule> rules = readObjects("BusinessRule", BusinessRule.class, editor);
//        editor.setBom(boms);
//        editor.setRules(rules);
//        // List<RuleDefinition> controls = readObjects(editor, "RuleDefinition",
//        // RuleDefinition.class);
//        //
//        // editor.setComponents(components);
//        // editor.setControls(controls);
//        // for (Component component : components) {
//        // List<NameValuePair> componentKeys = getXomMapper().flattenKeys(editor,
//        // component);
//        // NameValuePair[] keysArray = componentKeys.toArray(new NameValuePair[] {});
//        // List<Field> fields = readObjects("Field", Field.class, keysArray);
//        // component.setFields(fields);
//        // }
//        for (BOM bom : boms) {
//            List<NameValuePair> bomKeys = this.flattenKeys( bom,editor);
//         
//            List<BOMClass> bomClasses = readObjects("BOMClass", BOMClass.class, bomKeys);
//            bom.setBomClasses(bomClasses);
//            for (BOMClass bomClass : bomClasses) {
//                List<NameValuePair> bomClassKeys = this.flattenKeys( bomClass, bom);
//              
//                List<BOMElement> bomElements = readObjects("BOMElement", BOMElement.class, bomClassKeys);
//                bomClass.setBomElements(bomElements);
//            }
//        }
//        for (BusinessRule businessRule : rules) {
//            List<NameValuePair> ruleKeys = this.flattenKeys( businessRule, editor);
//        
//            List<BusinessRuleStatement> businessRuleStatements = readObjects("BusinessRuleStatement", BusinessRuleStatement.class, ruleKeys);
//            businessRule.setBusinessRuleStatements(businessRuleStatements);
//            for (BusinessRuleStatement businessRuleStatement : businessRuleStatements) {
//                parseBusinessRuleStatement(businessRule, businessRuleStatement);
//            }
//            // List<RuleDefinition> ruleDefinitions=
//            // readObjects("RuleDefinition", RuleDefinition.class, keysArray);
//            // List<RuleCondition> ruleConditions= readObjects("RuleCondition",
//            // RuleCondition.class, keysArray);
//            // List<RuleAction> ruleActions= readObjects("RuleAction",
//            // RuleAction.class, keysArray);
//            // businessRule.setRuleDefinitions(ruleDefinitions);
//            // businessRule.setRuleConditions(ruleConditions);
//            // businessRule.setRuleActions(ruleActions);
//        }
//    }
//
//    public BusinessRule parseBusinessRuleStatement(BusinessRule businessRule, BusinessRuleStatement businessRuleStatement) {
//        String term = businessRuleStatement.getDefinitionTerm();
//        if (term != null && term.trim().length() > 0) {
//            List<RuleDefinition> ruleDefinitions = businessRule.getRuleDefinitions();
//            RuleDefinition ruleDefinition = new RuleDefinition();
//            ruleDefinition.setTerm(businessRuleStatement.getDefinitionTerm());
//            ruleDefinition.setReferenceElementTerm(businessRuleStatement.getDefinitionReferenceElementTerm());
//            ruleDefinition.setReferenceHolderTerm(businessRuleStatement.getDefinitionReferenceHolderTerm());
//            ruleDefinition.setDefinitionType(businessRuleStatement.getDefinitionType());
//            ruleDefinitions.add(ruleDefinition);
//        }
//        RuleCondition ruleCondition = new RuleCondition();
//        boolean conditionFound = false;
//        if (businessRuleStatement.getConditionTerm() != null && businessRuleStatement.getConditionTerm().trim().length() > 0) {
//            ruleCondition.setTerm(businessRuleStatement.getConditionTerm());
//            conditionFound = true;
//        }
//        if (businessRuleStatement.getConditionReferenceTerm() != null && businessRuleStatement.getConditionReferenceTerm().trim().length() > 0) {
//            ruleCondition.setReferenceTerm(businessRuleStatement.getConditionReferenceTerm());
//            conditionFound = true;
//        }
//        if (businessRuleStatement.getConditionalOperator() != null && businessRuleStatement.getConditionalOperator().trim().length() > 0) {
//            ruleCondition.setConditionalOperator(businessRuleStatement.getConditionalOperator());
//            conditionFound = true;
//        }
//        if (businessRuleStatement.getConditionValue() != null && businessRuleStatement.getConditionValue().trim().length() > 0) {
//            ruleCondition.setValue(businessRuleStatement.getConditionValue());
//            conditionFound = true;
//        }
//        if (businessRuleStatement.getConcatinationOperator() != null && businessRuleStatement.getConcatinationOperator().trim().length() > 0) {
//            ruleCondition.setConcatinationOperator(businessRuleStatement.getConcatinationOperator());
//            conditionFound = true;
//        }
//        if (conditionFound) {
//            List<RuleCondition> ruleConditions = businessRule.getRuleConditions();
//            ruleConditions.add(ruleCondition);
//        }
//        RuleAction ruleAction = new RuleAction();
//        boolean actionFound = false;
//        if (businessRuleStatement.getActionTerm() != null && businessRuleStatement.getActionTerm().trim().length() > 0) {
//            ruleAction.setTerm(businessRuleStatement.getActionTerm());
//            actionFound = true;
//        }
//        if (businessRuleStatement.getActionReferenceTerm() != null && businessRuleStatement.getActionReferenceTerm().trim().length() > 0) {
//            ruleAction.setReferenceTerm(businessRuleStatement.getActionReferenceTerm());
//            actionFound = true;
//        }
//        if (businessRuleStatement.getActionValue() != null && businessRuleStatement.getActionValue().trim().length() > 0) {
//            ruleAction.setValue(businessRuleStatement.getActionValue());
//            actionFound = true;
//        }
//        if (actionFound) {
//            List<RuleAction> ruleActions = businessRule.getRuleActions();
//            ruleActions.add(ruleAction);
//        }
//        System.out.println(businessRule);
//        return businessRule;
//    }
}
