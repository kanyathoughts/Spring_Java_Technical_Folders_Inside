package fw2.hhs.rules.factories

import fw2.hhs.decisiontables.DecisionTable
import fw2.hhs.rules.BOM
import fw2.hhs.rules.BOMClass
import fw2.hhs.rules.BOMElement
import fw2.hhs.rules.BusinessRule
import fw2.hhs.rules.RuleProject
import fw2.orm.xlsx.io.Horizontal
import fw2.orm.xlsx.io.Location
import fw2.orm.xlsx.io.NameValuePair
import fw2.orm.xlsx.io.Orientation
import fw2.orm.xlsx.io.SheetLayout
import fw2.orm.xlsx.io.SizeType
import fw2.orm.xlsx.io.TableLayout
import fw2.orm.xlsx.io.VariableSize
import fw2.orm.xlsx.io.Vertical
import fw2.orm.xlsx.io.WorkbookReader
import fw2.orm.xlsx.io.mapper.Mapper
import java.io.IOException
import java.io.InputStream
import java.util.List
import org.apache.poi.ss.usermodel.Sheet
import fw2.orm.xlsx.io.FixedSize
import fw2.hhs.rules.Variable

class BoltRuleProjectXlsxReader extends WorkbookReader<RuleProject> {
	new() {
		super()
	}

	new(InputStream workbookInputStream, Mapper mapper) throws IOException {
		super(workbookInputStream, mapper)
	}

//def public RuleProject readRuleProject(String name) throws Exception {
//	val RuleProject dummy = new RuleProject();
//	dummy.setRuleProjectName(name);
//	val List<RuleProject> editors = readObjects("RuleProject", RuleProject.class, dummy);
//	for (RuleProject editor : editors) {
//		populateRuleProject(editor);
//		return editor;
//	}
//	return null;
//}
//def public List<RuleProject> readRuleProjects() throws Exception {
//	val List<RuleProject> editors = readObjects("RuleProject", RuleProject.class);
//	for (RuleProject editor : editors) {
//		populateRuleProject(editor);
//	}
//	return editors;
//}
	def private void populateRuleProject1(RuleProject project) throws Exception {
//			val ruleProject = this.readObjects(RuleProject.simpleName, typeof(RuleProject), project)
//	project
//		val boltRequirements = this.readObjects(BoltRequirement.simpleName, typeof(BoltRequirement), project)
//		val boltDiscoveredArtifacts= this.readObjects(BoltDiscoveredArtifact.simpleName, typeof(BoltDiscoveredArtifact), project)
//		val boltComponents = this.readObjects(BoltComponent.simpleName, typeof(BoltComponent), pkg)
//		val boltDataTypes = this.readObjects(BoltDataType.simpleName, typeof(BoltDataType), project)
//		val boltCodeTables = this.readObjects(BoltCodeTable.simpleName, typeof(BoltCodeTable), pkg)
//		project.requirements.addAll(boltRequirements);
//		project.discoveredArtifacts.addAll(boltDiscoveredArtifacts);
//		pkg.objects.addAll(boltObjects)
//		pkg.components.addAll(boltComponents)
//		pkg.dataTypes.addAll(boltDataTypes)
//		pkg.codeTables.addAll(boltCodeTables)
//
//		for (boltObject : boltObjects) {
//			populateBoltObject(boltObject)
//		}
//
//		for (boltComponent : boltComponents) {
//			populateBoltComponent(boltComponent)
//		}
//
//		for (codeTable : boltCodeTables) {
//			populateBoltCodeTable(codeTable)
//		}
	}

//	def private void populateBoltProject(BoltProject model) throws Exception {
//		val List<BoltPackage> packages = this.readObjects(BoltPackage.simpleName, typeof(BoltPackage), model)
//		model.getPackages().addAll(packages)
//		for (BoltPackage pkg : packages) {
//			populateBoltPackage(pkg)
//		}
//	}
//
//	def private void populateBoltComponent(BoltComponent boltComponent) throws Exception {
//		val fields = this.readObjects(BoltField.simpleName, typeof(BoltField), boltComponent)
//		boltComponent.fields.addAll(fields)
//	}
//
//	def private void populateBoltCodeTable(BoltCodeTable boltCodeTable) throws Exception {
//		val valueSet = this.readObjects(BoltKeyValuePair.simpleName, typeof(BoltKeyValuePair), boltCodeTable)
//		boltCodeTable.valueSet.addAll(valueSet)
//	}
//
//	def private void populateBoltObject(BoltObject boltObject) throws Exception {
//		val List<BoltAttribute> attributes = this.readObjects(BoltAttribute.simpleName, typeof(BoltAttribute),
//			boltObject)
//		boltObject.getAttributes().addAll(attributes)
//	}
//
//	override List<BoltProject> readObjects() throws Exception {
//		val Sheet sheet = this.workBook.getSheet(BoltProject.simpleName)
//		val nextgen.superbolt.util.BoltProjectRequirementsSheetReader  reader = new nextgen.superbolt.util.BoltProjectRequirementsSheetReader (this, sheet, this.mapper)
//		val List<BoltProject> models = reader.readObjects(typeof(BoltProject))
//		for (BoltProject model : models) {
//			this.populateBoltProject(model)
//		}
//		return models
//	}
////
//	override List<BoltProject> readObjects(List<NameValuePair> keys) throws Exception {
//		val Sheet sheet = this.workBook.getSheet(BoltProject.simpleName)
//		val nextgen.superbolt.util.BoltProjectRequirementsSheetReader reader = new nextgen.superbolt.util.BoltProjectRequirementsSheetReader(this, sheet, this.mapper)
//		val List<BoltProject> models = reader.readObjects(typeof(BoltProject), keys)
//		for (BoltProject model : models) {
//			this.populateBoltProject(model)
//		}
//		return models
//	}
//
//	def private <T> List<T> readObjects(String sheetName, Class<T> type, Object parentObject) throws Exception {
//		val List<NameValuePair> keys = this.mapper.flattenReferenceKeys(type, parentObject)
//		val Sheet sheet = this.workBook.getSheet(sheetName)
//		val nextgen.superbolt.util.BoltProjectRequirementsSheetReader  reader = new nextgen.superbolt.util.BoltProjectRequirementsSheetReader (this, sheet, this.mapper)
//		return reader.readObjects(type, keys)
//	}
	override Object lookupObject(String sheetName, List<NameValuePair> lookupKeys) {
		// TODO Auto-generated method stub
		return null
	}

	override readObjects() throws Exception {
		val Sheet sheet = this.workBook.getSheet(RuleProject.simpleName)
		val BoltRuleProjectSheetReader reader = new BoltRuleProjectSheetReader(this, sheet, this.mapper)
		val List<RuleProject> models = reader.readObjects(typeof(RuleProject))
		for (RuleProject model : models) {
			this.populateRuleProject(model)
		}
//		}
		return models
	}

	def populateRuleProject(RuleProject ruleProject) throws Exception {

		val Sheet bomSheet = this.workBook.getSheet(BOM.simpleName)
		val BoltRuleProjectSheetReader bomReader = new BoltRuleProjectSheetReader(this, bomSheet, this.mapper)
		val List<BOM> boms = bomReader.readObjects(typeof(BOM))
		for (BOM model : boms) {
			this.populateBOM(model)
		}
		ruleProject.bom.addAll(boms)

		val Sheet paramSheet = this.workBook.getSheet(Variable.simpleName)
		val BoltRuleProjectSheetReader paramReader = new BoltRuleProjectSheetReader(this, paramSheet, this.mapper)
		val List<Variable> variableSet = paramReader.readObjects(typeof(Variable))
		ruleProject.variableSet.addAll(variableSet)

		val Sheet rulesSheet = this.workBook.getSheet(BusinessRule.simpleName)

		val Location location = new Location(0, 0);
		var List<BusinessRule> rules = newArrayList
		val SheetLayout businessRuleSheetLayout = getBusinessRuleSheetLayout();
		val BoltRuleProjectSheetReader businessRuleReader = new BoltRuleProjectSheetReader(this, rulesSheet,
			businessRuleSheetLayout, this.mapper)
		rules = businessRuleReader.readObjects(rules, typeof(BusinessRule), ruleProject, location);
//		for (BOM model : boms) {
//			this.populateBOM(model)
//		}
		ruleProject.rules.addAll(rules)

		val Sheet decisionSheet = this.workBook.getSheet(DecisionTable.simpleName)

		val Location decisionLocation = new Location(0, 0);
		var List<DecisionTable> decisions = newArrayList
		val SheetLayout businessDecisionSheetLayout = getDecisionRuleSheetLayout();
		val BoltDecisionProjectSheetReader businessDecisionReader = new BoltDecisionProjectSheetReader(this,
			decisionSheet, businessDecisionSheetLayout, this.mapper)
		decisions = businessDecisionReader.readObjects(decisions, typeof(DecisionTable), ruleProject, decisionLocation);
		ruleProject.decisionRules.addAll(decisions)

//		val Sheet ruleSetParameterSheet = workBook.getSheet("RuleSetParameter");
//		val RuleProjectSheetReader ruleSetParameterSheetReader = new RuleProjectSheetReader(ruleSetParameterSheet,
//			this.getRepository());
//		val List<RuleSetParameter> ruleSetParameterList = ruleSetParameterSheetReader.readObjects(
//			RuleSetParameter.class, ruleProject);
//		ruleProject.setRuleSetParameters(ruleSetParameterList);
//		val SheetLayout businessRuleSheetLayout = getBusinessRuleSheetLayout();
//		val Sheet sheet = workBook.getSheet("BusinessRule - " + ruleProject.getRuleProjectName());
//		val BusinessRuleSheetReader ruleSheetReader = new BusinessRuleSheetReader(sheet, businessRuleSheetLayout,
//			this.getRepository());
//		val Location location = new Location(0, 0);
//		val List<BusinessRule> result = new ArrayList<BusinessRule>();
//		ruleSheetReader.readObjects(result, BusinessRule.class, ruleProject, location);
//		ruleProject.setRules(result);
//
//		val Sheet bomSheet = workBook.getSheet("BOM");
//		val RuleProjectSheetReader bomSheetReader = new RuleProjectSheetReader(bomSheet, this.getRepository());
//
//		val List<BOM> bomList = bomSheetReader.readObjects(BOM.class, ruleProject);
//		for (BOM bom : bomList) {
//			val Sheet bomClassSheet = workBook.getSheet("BOMClass");
//			val RuleProjectSheetReader bomClassSheetReader = new RuleProjectSheetReader(bomClassSheet,
//				this.getRepository());
//
//			val List<BOMClass> bomClasses = bomClassSheetReader.readObjects(BOMClass.class, bom);
//			bom.setBomClasses(bomClasses);
//			for (BOMClass bomClass : bomClasses) {
//				val Sheet bomElementSheet = workBook.getSheet("BOMElement");
//				val RuleProjectSheetReader bomElementReader = new RuleProjectSheetReader(bomElementSheet,
//					this.getRepository());
//
//				val List<BOMElement> bomElements = bomElementReader.readObjects(BOMElement.class, bomClass);
//				bomClass.setBomElements(bomElements);
//			}
//		}
//		ruleProject.setBom(bomList);
		ruleProject
	}

	def populateBOM(BOM bom) {
		val Sheet sheet = this.workBook.getSheet(BOMClass.simpleName)
		val BoltRuleProjectSheetReader reader = new BoltRuleProjectSheetReader(this, sheet, this.mapper)
		val List<BOMClass> models = reader.readObjects(typeof(BOMClass))
		models.forEach [ p1, p2 |
			p1.populateBOMClass
		]

		bom.bomClasses.addAll(models)
	}

	def populateBOMClass(BOMClass bomClass) {
		val Sheet sheet = this.workBook.getSheet(BOMElement.simpleName)
		val BoltRuleProjectSheetReader reader = new BoltRuleProjectSheetReader(this, sheet, this.mapper)
		val List<BOMElement> models = reader.readObjects(typeof(BOMElement)) // Need to consider bomClass name while adding elements
		for (BOMElement model : models) {
			if(bomClass.className.equals(model.className))
				bomClass.bomElements.add(model)
		}
	}

	override readObjects(List<NameValuePair> keys) throws Exception
	{
		throw new UnsupportedOperationException("TODO: auto-generated method stub")
	}

	def private <T> List<T> readObjects(String sheetName, Class<T> type, Object parentObject) throws Exception {
		val List<NameValuePair> keys = this.mapper.flattenReferenceKeys(type, parentObject)
		val Sheet sheet = this.workBook.getSheet(sheetName)
		val BoltRuleProjectSheetReader reader = new BoltRuleProjectSheetReader(this, sheet, this.mapper)
		return reader.readObjects(type, keys)
	}

	def SheetLayout getBusinessRuleSheetLayout() {
		val SheetLayout businessRuleSheetLayout = new SheetLayout("businessRuleSheetLayout");
		// businessRuleSheetLayout.setName("Horizontal");
		var SizeType sizeType = new VariableSize();
		businessRuleSheetLayout.setSizeType(sizeType);
		var Orientation orientation = new Horizontal();
		businessRuleSheetLayout.setOrientation(orientation);
		businessRuleSheetLayout.setOrigin(new Location(0, 0));
		var TableLayout ruleDefintiionLayout = new TableLayout("ruleDefinitionLayout");
		// ruleDefintiionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleDefintiionLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleDefintiionLayout.setOrientation(orientation);
		ruleDefintiionLayout.setOrigin(new Location(0, 0));
		var TableLayout ruleConditionLayout = new TableLayout("ruleConditionLayout");
		// ruleConditionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleConditionLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleConditionLayout.setOrientation(orientation);
		ruleConditionLayout.setOrigin(new Location(0, 0));
		var TableLayout ruleActionLayout = new TableLayout("ruleActionLayout");
		// ruleActionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleActionLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleActionLayout.setOrientation(orientation);
		ruleActionLayout.setOrigin(new Location(0, 0));

		businessRuleSheetLayout.getTableLayouts().add(ruleDefintiionLayout);
		businessRuleSheetLayout.getTableLayouts().add(ruleConditionLayout);
		businessRuleSheetLayout.getTableLayouts().add(ruleActionLayout);
		return businessRuleSheetLayout;
	}

	def SheetLayout getDecisionRuleSheetLayout() {
		val SheetLayout decisionRuleSheetLayout = new SheetLayout("decisionRuleSheetLayout");
		// businessRuleSheetLayout.setName("Horizontal");
		var SizeType sizeType = new VariableSize();
		decisionRuleSheetLayout.setSizeType(sizeType);
		var Orientation orientation = new Horizontal();
		decisionRuleSheetLayout.setOrientation(orientation);
		decisionRuleSheetLayout.setOrigin(new Location(0, 0));

		var TableLayout ruleDefinitionLayout = new TableLayout("decisionRuleDefinitionLayout");
		// ruleDefintiionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleDefinitionLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleDefinitionLayout.setOrientation(orientation);
		ruleDefinitionLayout.setOrigin(new Location(0, 0));

		var TableLayout ruleConditionLayout = new TableLayout("decisionRuleConditionLayout");
		// ruleConditionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleConditionLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleConditionLayout.setOrientation(orientation);
		ruleConditionLayout.setOrigin(new Location(0, 0));

		var TableLayout ruleConditionColumnLayout = new TableLayout("decisionRuleConditionColumnLayout");
		// ruleConditionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleConditionColumnLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleConditionColumnLayout.setOrientation(orientation);
		ruleConditionColumnLayout.setOrigin(new Location(0, 0));

		var TableLayout ruleActionColumnLayout = new TableLayout("decisionRuleActionColumnLayout");
		// ruleActionLayout.setName("Vertical");
		sizeType = new VariableSize();
		ruleActionColumnLayout.setSizeType(sizeType);
		orientation = new Vertical();
		ruleActionColumnLayout.setOrientation(orientation);
		ruleActionColumnLayout.setOrigin(new Location(0, 0));

		decisionRuleSheetLayout.getTableLayouts().add(ruleDefinitionLayout);
		decisionRuleSheetLayout.getTableLayouts().add(ruleConditionLayout);
		decisionRuleSheetLayout.getTableLayouts().add(ruleConditionColumnLayout);
		decisionRuleSheetLayout.getTableLayouts().add(ruleActionColumnLayout);
		return decisionRuleSheetLayout;
	}
}
