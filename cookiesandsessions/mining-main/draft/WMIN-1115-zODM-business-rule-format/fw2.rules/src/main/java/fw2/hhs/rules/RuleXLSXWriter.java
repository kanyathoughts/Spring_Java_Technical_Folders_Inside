package fw2.hhs.rules;
//
//import java.io.FileInputStream;
//import java.io.InputStream;
//import java.util.ArrayList;
//import java.util.Collection;
//import java.util.HashMap;
//import java.util.List;
//
//import org.apache.poi.ss.usermodel.Sheet;
//
//import fw2.access.FW2Repository;
//import fw2.hhs.decisiontables.DecisionTableXLSReader;
//import fw2.hhs.decisiontables.DecisionTable;
//import fw2.orm.NameValuePair;
//import fw2.util.CollectionUtil;
//import fw2.xlsx.XLSXWriter;

public class RuleXLSXWriter {//extends XLSXWriter {
//    public RuleXLSXWriter(String fileName) {
//        super(fileName);
//        // TODO Auto-generated constructor stub
//    }
//
//    public void writeRuleProject(RuleProject editor) throws Exception {
//        try {
//            List<NameValuePair> flattenedRuleProject = this.flatten(editor);
//            Sheet sheet = this.createSheet("RuleProject");
//            this.createHeaderRow(sheet, flattenedRuleProject);
//            this.createValueRow(sheet, flattenedRuleProject);
//            List<List<NameValuePair>> flattenedComponentRows = this.flattenRuleSetParameters(editor);
//            sheet = this.createSheet("RuleSetParameter");
//            List<NameValuePair> headerRow = CollectionUtil.car(flattenedComponentRows);
//            if (headerRow != null) {
//                this.createHeaderRow(sheet, headerRow);
//                for (List<NameValuePair> row : flattenedComponentRows) {
//                    this.createValueRow(sheet, row);
//                }
//            }
//            List<List<NameValuePair>> flattenedBOMRows = this.flattenBOM(editor);
//            sheet = this.createSheet("BOM");
//            headerRow = CollectionUtil.car(flattenedBOMRows);
//            if (headerRow != null) {
//                this.createHeaderRow(sheet, headerRow);
//                for (List<NameValuePair> row : flattenedBOMRows) {
//                    this.createValueRow(sheet, row);
//                }
//            }
//            List<List<NameValuePair>> flattenedBOMClassRows = this.flattenBOMClass(editor);
//            sheet = this.createSheet("BOMClass");
//            headerRow = CollectionUtil.car(flattenedBOMClassRows);
//            if (headerRow != null) {
//                this.createHeaderRow(sheet, headerRow);
//                for (List<NameValuePair> row : flattenedBOMClassRows) {
//                    this.createValueRow(sheet, row);
//                }
//            }
//            List<List<NameValuePair>> flattenedBOMElementRows = this.flattenElement(editor);
//            sheet = this.createSheet("BOMElement");
//            headerRow = CollectionUtil.car(flattenedBOMElementRows);
//            if (headerRow != null) {
//                this.createHeaderRow(sheet, headerRow);
//                for (List<NameValuePair> row : flattenedBOMElementRows) {
//                    this.createValueRow(sheet, row);
//                }
//            }
//            List<List<NameValuePair>> flattenedRuleRows = this.flattenRules(editor);
//            sheet = this.createSheet("BusinessRule");
//            headerRow = CollectionUtil.car(flattenedRuleRows);
//            if (headerRow != null) {
//                this.createHeaderRow(sheet, headerRow);
//                for (List<NameValuePair> row : flattenedRuleRows) {
//                    this.createValueRow(sheet, row);
//                }
//            }
//            List<List<NameValuePair>> flattenedRuleDefinitionRows = this.flattenBusinessRuleStatementRows(editor);
//            sheet = this.createSheet("BusinessRuleStatement");
//            headerRow = CollectionUtil.car(flattenedRuleDefinitionRows);
//            if (headerRow != null) {
//                this.createHeaderRow(sheet, headerRow);
//                for (List<NameValuePair> row : flattenedRuleDefinitionRows) {
//                    this.createValueRow(sheet, row);
//                }
//            }
//            // List<List<NameValuePair>> flattenedRuleDefinitionRows =
//            // this.flattenRuleDefinitionRows(editor);
//            // sheet = this.createSheet("RuleDefinition");
//            // headerRow = CollectionUtil.car(flattenedRuleDefinitionRows);
//            // if (headerRow != null) {
//            // this.createHeaderRow(sheet, headerRow);
//            // for (List<NameValuePair> row : flattenedRuleDefinitionRows) {
//            // this.createValueRow(sheet, row);
//            // }
//            // }
//            // List<List<NameValuePair>> flattenedRuleConditionRows =
//            // this.flattenRuleConditionRows(editor);
//            // sheet = this.createSheet("RuleCondition");
//            // headerRow = CollectionUtil.car(flattenedRuleConditionRows);
//            // if (headerRow != null) {
//            // this.createHeaderRow(sheet, headerRow);
//            // for (List<NameValuePair> row : flattenedRuleConditionRows) {
//            // this.createValueRow(sheet, row);
//            // }
//            // }
//            //
//            //
//            // List<List<NameValuePair>> flattenedRuleActionRows =
//            // this.flattenRuleActionRows(editor);
//            // sheet = this.createSheet("RuleAction");
//            // headerRow = CollectionUtil.car(flattenedRuleActionRows);
//            // if (headerRow != null) {
//            // this.createHeaderRow(sheet, headerRow);
//            // for (List<NameValuePair> row : flattenedRuleActionRows) {
//            // this.createValueRow(sheet, row);
//            // }
//            // }
//        } finally {
//            this.close();
//        }
//    }
//
//    private List<List<NameValuePair>> flattenBusinessRuleStatementRows(RuleProject editor) throws Exception {
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BusinessRule> rules = editor.getRules();
//        List<NameValuePair> editorKeys = this.flattenKeys(editor);
//        for (BusinessRule businessRule : rules) {
//            List<NameValuePair> businessRuleKeys = this.flattenKeys(businessRule);
//            List<BusinessRuleStatement> businessRuleStatements = businessRule.getBusinessRuleStatements();
//            for (BusinessRuleStatement businessRuleStatement : businessRuleStatements) {
//                List<NameValuePair> flattened = new ArrayList<NameValuePair>();
//                flattened.addAll(editorKeys);
//                flattened.addAll(businessRuleKeys);
//                flattened.addAll(this.flatten(businessRuleStatement, businessRule));
//                result.add(flattened);
//            }
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenRuleDefinitionRows(RuleProject editor) throws Exception {
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BusinessRule> rules = editor.getRules();
//        List<NameValuePair> editorKeys = this.flattenKeys(editor);
//        for (BusinessRule businessRule : rules) {
//            List<NameValuePair> businessRuleKeys = this.flattenKeys(businessRule);
//            Collection<RuleDefinition> ruleDefinitions = businessRule.getRuleDefinitions();
//            for (RuleDefinition ruleDefinition : ruleDefinitions) {
//                List<NameValuePair> flattened = new ArrayList<NameValuePair>();
//                flattened.addAll(editorKeys);
//                flattened.addAll(businessRuleKeys);
//                flattened.addAll(this.flatten(ruleDefinition, businessRule));
//                result.add(flattened);
//            }
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenRuleActionRows(RuleProject editor) throws Exception {
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BusinessRule> rules = editor.getRules();
//        List<NameValuePair> editorKeys = this.flattenKeys(editor);
//        for (BusinessRule businessRule : rules) {
//            List<NameValuePair> businessRuleKeys = this.flattenKeys(businessRule);
//            Collection<RuleAction> ruleActions = businessRule.getRuleActions();
//            for (RuleAction ruleAction : ruleActions) {
//                List<NameValuePair> flattened = new ArrayList<NameValuePair>();
//                flattened.addAll(editorKeys);
//                flattened.addAll(businessRuleKeys);
//                flattened.addAll(this.flatten(ruleAction, businessRule));
//                result.add(flattened);
//            }
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenRuleConditionRows(RuleProject editor) throws Exception {
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BusinessRule> rules = editor.getRules();
//        List<NameValuePair> editorKeys = this.flattenKeys(editor);
//        for (BusinessRule businessRule : rules) {
//            List<NameValuePair> businessRuleKeys = this.flattenKeys(businessRule);
//            Collection<RuleCondition> ruleConditions = businessRule.getRuleConditions();
//            for (RuleCondition ruleCondition : ruleConditions) {
//                List<NameValuePair> flattened = new ArrayList<NameValuePair>();
//                flattened.addAll(editorKeys);
//                flattened.addAll(businessRuleKeys);
//                flattened.addAll(this.flatten(ruleCondition, businessRule));
//                result.add(flattened);
//            }
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenRules(RuleProject editor) throws Exception {
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BusinessRule> rules = editor.getRules();
//        // List<NameValuePair> editorKeys = getXomMapper().flattenKeys(editor);
//        for (BusinessRule businessRule : rules) {
//            List<NameValuePair> flattened = this.flatten(businessRule, editor);
//            result.add(flattened);
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenElement(RuleProject editor) throws Exception {
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BOM> components = editor.getBom();
//        List<NameValuePair> editorKeys = this.flattenKeys(editor);
//        for (BOM component : components) {
//            Collection<BOMClass> bomClasses = component.getBomClasses();
//            List<NameValuePair> bomClassKeys = this.flattenKeys(component);
//            for (BOMClass bomClass : bomClasses) {
//                Collection<BOMElement> bomElements = bomClass.getBomElements();
//                for (BOMElement bomElement : bomElements) {
//                    List<NameValuePair> flattened = new ArrayList<NameValuePair>();
//                    flattened.addAll(editorKeys);
//                    flattened.addAll(bomClassKeys);
//                    flattened.addAll(this.flatten(bomElement, bomClass));
//                    result.add(flattened);
//                }
//            }
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenBOMClass(RuleProject editor) throws Exception {
//        // TODO Auto-generated method stub
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BOM> components = editor.getBom();
//        List<NameValuePair> editorKeys = this.flattenKeys(editor);
//        for (BOM component : components) {
//            Collection<BOMClass> fields = component.getBomClasses();
//            for (BOMClass field : fields) {
//                List<NameValuePair> flattened = new ArrayList<NameValuePair>();
//                flattened.addAll(editorKeys);
//                flattened.addAll(this.flatten(field, component));
//                result.add(flattened);
//            }
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenBOM(RuleProject editor) throws Exception {
//        // TODO Auto-generated method stub
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<BOM> components = editor.getBom();
//        for (BOM component : components) {
//            List<NameValuePair> flattened = this.flatten(component, editor);
//            result.add(flattened);
//        }
//        return result;
//    }
//
//    private List<List<NameValuePair>> flattenRuleSetParameters(RuleProject editor) throws Exception {
//        // TODO Auto-generated method stub
//        List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
//        Collection<RuleSetParameter> components = editor.getRuleSetParameters();
//        for (RuleSetParameter component : components) {
//            List<NameValuePair> flattened = this.flatten(component, editor);
//            result.add(flattened);
//        }
//        return result;
//    }
//
//    public static void test(String args[]) throws Exception {
//        FW2Repository.getInstance();
//        RuleProject ruleProject = createRuleProject();
//        // InputStream mapFileStream = new FileInputStream("Editor.xlsx");
//        // RuleXLSXReader xlsxReader = new RuleXLSXReader(mapFileStream);
//        // List<RuleProject> editors = xlsxReader.readRuleProjects();
//        // mapFileStream.close();
//        // RuleXLSXWriter editorXLSXWriter = new
//        // RuleXLSXWriter("RuleProject.xlsx");
//        // editorXLSXWriter.writeRuleProject(ruleProject);
//        InputStream mapFileStream = new FileInputStream("RuleProject.xlsx");
//        RuleXLSXReader xlsxReader = new RuleXLSXReader(mapFileStream);
//        List<RuleProject> ruleProjects = xlsxReader.readRuleProjects();
//        mapFileStream.close();
//        // //
//        for (RuleProject ruleProject2 : ruleProjects) {
//            RuleManager ruleManager = new RuleManager();
//            ruleManager.createProject(ruleProject2);
//            ruleManager.createRuleProject(ruleProject2);
//            List<BOM> bomList = ruleProject2.getBom();
//            for (BOM bom : bomList) {
//                ruleManager.createBOM(ruleProject2, bom);
//            }
//            List<BusinessRule> businessRules = ruleProject2.getRules();
//            // for (BusinessRule businessRule : businessRules)
//            {
//                ruleManager.createBRL(ruleProject2.getRepositoryLocation(), ruleProject2.getRuleProjectName(), businessRules);
//            }
//            DecisionTableXLSReader decisionTableXLSReader = new DecisionTableXLSReader();
//            HashMap<String, List<DecisionTable>> decisionTableMap = decisionTableXLSReader.read("c:\\POC\\Decision Table Template.xlsx");
//            List<DecisionTable> decisionTableList = decisionTableMap.get(ruleProject2.getRuleProjectName());
//            if (decisionTableList != null) {
//                for (DecisionTable decisionTable : decisionTableList) {
//                    ruleManager.createDecisionTable(ruleProject2, decisionTable);
//                }
//            }
//        }
//    }
//
//    private static RuleProject createRuleProject() {
//        // TODO Auto-generated method stub
//        EclipseProject eclipseProject = new EclipseProject();
//        eclipseProject.setRepositoryLocation("C:\\POC\\ruleRepository");
//        eclipseProject.setProjectName("TestProject");
//        RuleProject ruleProject = new RuleProject();
//        ruleProject.setRepositoryLocation("C:\\POC\\ruleRepository");
//        ruleProject.setRuleProjectName("TestProject");
//        RuleSetParameter ruleSetParameter1 = new RuleSetParameter();
//        ruleSetParameter1.setDataType("int");
//        ruleSetParameter1.setName("param1");
//        RuleSetParameter ruleSetParameter2 = new RuleSetParameter();
//        ruleSetParameter2.setDataType("int");
//        ruleSetParameter2.setName("param2");
//        ArrayList<RuleSetParameter> ruleSetParameterList = new ArrayList<RuleSetParameter>();
//        ruleSetParameterList.add(ruleSetParameter1);
//        ruleSetParameterList.add(ruleSetParameter2);
//        ruleProject.setRuleSetParameters(ruleSetParameterList);
//        List<BOM> bomList = new ArrayList<BOM>();
//        BOM bom1 = new BOM();
//        bom1.setBomName("model1");
//        BOMClass bomClass1 = new BOMClass();
//        bomClass1.setClassName("TestClassOne");
//        bomClass1.setPackageName("com.deloitte.testproject");
//        bomClass1.setVerbalization("test class one");
//        BOMElement bomElement1 = new BOMElement();
//        bomElement1.setDataType("int");
//        bomElement1.setElementName("elementOne");
//        bomElement1.setVerbalization("element one");
//        bomElement1.setIsStatic("Y");
//        BOMElement bomElement2 = new BOMElement();
//        bomElement2.setDataType("int");
//        bomElement2.setElementName("elementTwo");
//        bomElement2.setVerbalization("element two");
//        List<BOMElement> bomElements = new ArrayList<BOMElement>();
//        bomElements.add(bomElement1);
//        bomElements.add(bomElement2);
//        bomClass1.setBomElements(bomElements);
//        BOMClass bomClass2 = new BOMClass();
//        bomClass2.setClassName("TestClassTwo");
//        bomClass2.setPackageName("com.deloitte.testproject");
//        bomClass2.setVerbalization("test class two");
//        bomClass2.setBomElements(bomElements);
//        List<BOMClass> bomClasses = new ArrayList<BOMClass>();
//        bomClasses.add(bomClass1);
//        bomClasses.add(bomClass2);
//        bom1.setBomClasses(bomClasses);
//        BOM bom2 = new BOM();
//        bom2.setBomName("model2");
//        bom2.setBomClasses(bomClasses);
//        bomList.add(bom1);
//        bomList.add(bom2);
//        ruleProject.setBom(bomList);
//        BusinessRule businessRule1 = new BusinessRule();
//        businessRule1.setRuleName("Test Rule 1");
//        businessRule1.setRulePackage("testrulePack");
//        BusinessRuleStatement businessRuleStatement = new BusinessRuleStatement();
//        businessRuleStatement.setDefinitionTerm("term");
//        businessRuleStatement.setDefinitionReferenceElementTerm("definitionReferenceTerm");
//        businessRuleStatement.setConditionTerm("conditionTerm");
//        businessRuleStatement.setConditionReferenceTerm("conditionReferenceTerm");
//        businessRuleStatement.setConditionalOperator("conditionalOperator");
//        businessRuleStatement.setConditionValue("value");
//        businessRuleStatement.setConcatinationOperator("concatinationOperator");
//        businessRuleStatement.setActionTerm("actionTerm");
//        businessRuleStatement.setActionReferenceTerm("actionReferenceTerm");
//        businessRuleStatement.setActionValue("actionValue");
//        List<BusinessRuleStatement> businessRuleStatements = new ArrayList<BusinessRuleStatement>();
//        businessRuleStatements.add(businessRuleStatement);
//        businessRule1.setBusinessRuleStatements(businessRuleStatements);
//        // RuleDefinition ruleDefinition1=new RuleDefinition();
//        // ruleDefinition1.setDefinitionNumber("1");
//        // ruleDefinition1.setTerm("term");
//        // ruleDefinition1.setReferenceTerm("reference term");
//        //
//        // RuleDefinition ruleDefinition2=new RuleDefinition();
//        // ruleDefinition2.setDefinitionNumber("2");
//        // ruleDefinition2.setTerm("term");
//        // ruleDefinition2.setReferenceTerm("reference term");
//        //
//        //
//        // List<RuleDefinition> ruleDefinitions=new ArrayList<RuleDefinition>();
//        // ruleDefinitions.add(ruleDefinition1);
//        // ruleDefinitions.add(ruleDefinition2);
//        //
//        //
//        // RuleCondition ruleCondition1=new RuleCondition();
//        // ruleCondition1.setConditionNumber("1");
//        // ruleCondition1.setTerm("term");
//        // ruleCondition1.setReferenceTerm("reference term");
//        // ruleCondition1.setValue("value1");
//        //
//        // RuleCondition ruleCondition2=new RuleCondition();
//        // ruleCondition2.setConditionNumber("2");
//        // ruleCondition2.setTerm("term");
//        // ruleCondition2.setReferenceTerm("reference term");
//        // ruleCondition2.setValue("value2");
//        //
//        // List<RuleCondition> ruleConditions=new ArrayList<RuleCondition>();
//        // ruleConditions.add(ruleCondition1);
//        // ruleConditions.add(ruleCondition2);
//        //
//        // RuleAction ruleAction1=new RuleAction();
//        // ruleAction1.setActionNumber("1");
//        // ruleAction1.setTerm("term");
//        // ruleAction1.setReferenceTerm("reference term");
//        // ruleAction1.setValue("value1");
//        //
//        // RuleAction ruleAction2=new RuleAction();
//        // ruleAction2.setActionNumber("2");
//        // ruleAction2.setTerm("term");
//        // ruleAction2.setReferenceTerm("reference term");
//        // ruleAction2.setValue("value2");
//        //
//        // List<RuleAction> ruleActions=new ArrayList<RuleAction>();
//        // ruleActions.add(ruleAction1);
//        // ruleActions.add(ruleAction2);
//        // businessRule1.setRuleDefinitions(ruleDefinitions);
//        // businessRule1.setRuleConditions(ruleConditions);
//        // businessRule1.setRuleActions(ruleActions);
//        BusinessRule businessRule2 = new BusinessRule();
//        businessRule2.setBusinessRuleStatements(businessRuleStatements);
//        businessRule2.setRuleName("Test Rule 2");
//        businessRule2.setRulePackage("testrulePack");
//        // businessRule2.setRuleDefinitions(ruleDefinitions);
//        // businessRule2.setRuleConditions(ruleConditions);
//        // businessRule2.setRuleActions(ruleActions);
//        List<BusinessRule> rules = new ArrayList<BusinessRule>();
//        rules.add(businessRule1);
//        rules.add(businessRule2);
//        ruleProject.setRules(rules);
//        return ruleProject;
//    }
//    // private static RuleProject createTestEditor() {
//    // TemporalEditor editor = new TemporalEditor();
//    // editor.setName("Alternate Payee Editor");
//    // Field field = new Field();
//    // field.setLabel("Start Date");
//    // field.setName("startDate");
//    // field.setType("Date");
//    // field.setId("startDateField_Id");
//    // field.setValue("");
//    // editor.setStartDateField(field);
//    // field = new Field();
//    // field.setLabel("End Date");
//    // field.setName("endDate");
//    // field.setType("Date");
//    // field.setId("endDateField_Id");
//    // field.setValue("");
//    // editor.setEndDateField(field);
//    // field = new Field();
//    // field.setLabel("Update Date");
//    // field.setName("updateDate");
//    // field.setType("Date");
//    // field.setId("updateDateField_Id");
//    // field.setValue("");
//    // editor.setUpdateDateField(field);
//    // Component component = new Component();
//    // component.setName("AlternatePayee.Name");
//    // component.setId("AlternatePayee.Name_id");
//    // editor.adddComponent(component);
//    // field = new Field();
//    // field.setLabel("First Name");
//    // field.setName("firstName");
//    // field.setType("text");
//    // field.setId("firstNameField_Id");
//    // field.setValue("");
//    // component.adddField(field);
//    // field = new Field();
//    // field.setLabel("Last Name");
//    // field.setName("lastName");
//    // field.setType("text");
//    // field.setId("lastNameField_Id");
//    // field.setValue("");
//    // component.adddField(field);
//    // field = new Field();
//    // field.setLabel("Middle Initial");
//    // field.setName("midddleInitial");
//    // field.setType("text");
//    // field.setId("midddleInitialField_Id");
//    // field.setValue("");
//    // component.adddField(field);
//    // field = new Field();
//    // field.setLabel("Social Security Number");
//    //
//    // field.setName("socialSecurityNumber");
//    // field.setType("text");
//    // field.setId("socialSecurityNumberField_Id");
//    // field.setValue("");
//    // component.adddField(field);
//    // Control control = new Control();
//    // control.setLabel("Update");
//    //
//    // control.setName("Update");
//    // control.setType("Submit");
//    // control.setId("UpdateField_Id");
//    // control.setValue("Update");
//    // control.setMethod("updateAuthRepChanges()");
//    // editor.adddControl(control);
//    // control = new Control();
//    // control.setLabel("Delete");
//    //
//    // control.setName("Delete");
//    // control.setType("Submit");
//    // control.setId("DeleteField_Id");
//    // control.setValue("Delete");
//    // control.setMethod("deleteAuthRepChanges()");
//    // editor.adddControl(control);
//    // return editor;
//    // }
}
