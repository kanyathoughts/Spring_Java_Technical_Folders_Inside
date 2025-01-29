package fw2.hhs.rules;
import java.util.List;

import fw2.orm.xlsx.io.NameValuePair;
//
//import java.io.FileInputStream;
//import java.io.IOException;
//import java.io.InputStream;
//import java.util.ArrayList;
//import java.util.List;
//
//import org.apache.poi.ss.usermodel.Sheet;
//
//import fw2.hhs.decisiontables.ActionColumn;
//import fw2.hhs.decisiontables.ConditionColumn;
//import fw2.hhs.decisiontables.DecisionTable;
//import fw2.hhs.rules.factories.BusinessRuleFactory;
//import fw2.hhs.rules.factories.DecisionTableFactory;
//import fw2.orm.xlsx.io.Factory;
//import fw2.orm.xlsx.io.InstanceFactory;
//import fw2.orm.xlsx.io.mapper.Mapper;
//import fw2.orm.xlsx.io.NameValuePair;
//import fw2.repository.FW2Repository;
//import fw2.repository.Repository;
//import fw2.orm.xlsx.io.Horizontal;
//import fw2.orm.xlsx.io.Location;
//import fw2.orm.xlsx.io.Orientation;
//import fw2.orm.xlsx.io.RepositoryImpl;
//import fw2.orm.xlsx.io.SheetLayout;
//import fw2.orm.xlsx.io.SheetReader;
//import fw2.orm.xlsx.io.SizeType;
//import fw2.orm.xlsx.io.TableLayout;
//import fw2.orm.xlsx.io.VariableSize;
//import fw2.orm.xlsx.io.Vertical;
import fw2.orm.xlsx.io.WorkbookReader;
//
public class RuleProjectReader extends WorkbookReader<RuleProject> {

	@Override
	public List<RuleProject> readObjects() throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<RuleProject> readObjects(List<NameValuePair> keys) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object lookupObject(String sheetName, List<NameValuePair> lookupKeys) {
		// TODO Auto-generated method stub
		return null;
	}
	
//	private class RuleProjectSheetReader extends SheetReader {
//		public RuleProjectSheetReader(Sheet sheet,Repository repository) {
//			super(sheet, repository);
//			// TODO Auto-generated constructor stub
//		}
//
//		@Override
//		public <T> InstanceFactory getInstanceFactory(Class<T> type) {
//			// TODO Auto-generated method stub
//			return new Factory<T>(this.getRepository());	
//		}
//	}
//
//	private class DecisionTableSheetReader extends SheetReader {
//		public DecisionTableSheetReader(Sheet sheet,Repository repository) {
//			super(sheet,repository);
//			// TODO Auto-generated constructor stub
//		}
//
//		public DecisionTableSheetReader(Sheet decisionTableSheet,
//				SheetLayout decisionTableLayout,Repository repository) {
//			super(decisionTableSheet, decisionTableLayout, repository);
//		}
//
//		@Override
//		public <T> InstanceFactory getInstanceFactory(Class<T> type) {
//			// TODO Auto-generated method stub
//			if (type == DecisionTable.class) {
//				return new DecisionTableFactory(this.getRepository());
//			}
//
//			else if (type == RuleDefinition.class) {
//				return new DecisionTableFactory(this.getRepository());
//			}
//
//			else if (type == RuleCondition.class) {
//				return new DecisionTableFactory(this.getRepository());
//			}
//
//			else if (type == RuleAction.class) {
//				return new DecisionTableFactory(this.getRepository());
//			} else if (type == ConditionColumn.class) {
//				return new DecisionTableFactory(this.getRepository());
//			} else if (type == ActionColumn.class) {
//				return new DecisionTableFactory(this.getRepository());
//			} else {
//				return new Factory<T>(this.getRepository());
//			}
//		}
//
//		public <T> T readObject(Class<T> type, Location location)
//				throws Exception {
//
//			// List<NameValuePair> keys = this.flattenKeys(parentObject);
//			this.rowReader.setRowIndex(location.getRowIndex());
//			
//			ArrayList<ArrayList<NameValuePair>> rows = this.layout
//					.readLogicalRows(this.rowReader);// , location);
//			ArrayList<NameValuePair> row = null;
//			if (rows != null && rows.size() > 0) {
//				row = rows.get(0);
//			}
//
//			T instance = this.createInstance(type, row);
//
//			List<TableLayout> tableLayouts = this.layout.getTableLayouts();
//			Orientation orientation = this.layout.getOrientation();
//			Location tableLocation = location;
//			if (orientation instanceof Horizontal) {
//				int rowIndex = layout.getOrigin().getRowIndex() + row.size()
//						+ 1;
//				int cellIndex = 0;
//				tableLocation = new Location(rowIndex, cellIndex);
//
//			} else if (orientation instanceof Vertical) {
//				int rowIndex = layout.getOrigin().getRowIndex() + row.size()
//						+ 2;
//				int cellIndex = layout.getOrigin().getCellIndex();
//				tableLocation = new Location(rowIndex, cellIndex);
//
//			}
//
//			for (TableLayout tableLayout : tableLayouts) {
//
//				tableLayout.setOrigin(tableLocation);
//				// ArrayList<NameValuePair> k = new ArrayList<NameValuePair>();
//				ArrayList<ArrayList<NameValuePair>> tableRows = tableLayout
//						.readLogicalRows(this.rowReader);
//
//				Orientation tableOrientation = tableLayout.getOrientation();
//				if (tableOrientation instanceof Horizontal) {
//					int rowIndex = layout.getOrigin().getRowIndex()
//							+ row.size() + 1;
//					int cellIndex = layout.getOrigin().getCellIndex()
//							+ row.size() - 1;
//					tableLocation = new Location(rowIndex, cellIndex);
//
//				} else if (tableOrientation instanceof Vertical) {
//					int rowIndex = tableLayout.getOrigin().getRowIndex()
//							+ tableRows.size() + 2;
//					int cellIndex = tableLayout.getOrigin().getCellIndex();
//					tableLocation = new Location(rowIndex, cellIndex);
//
//				}
//
//				if (tableLayout.getName().equalsIgnoreCase(
//						"conditionTermLayout")
//						|| tableLayout.getName().equalsIgnoreCase(
//								"actionTermLayout")) {
//					int rowIndex = tableLayout.getOrigin().getRowIndex();
//					int cellIndex = tableLayout.getOrigin().getCellIndex() + 3;
//					tableLocation = new Location(rowIndex, cellIndex);
//				}
//
//				if (tableLayout.getName().equalsIgnoreCase(
//						"conditionDataLayout")) {
//					int rowIndex = tableLocation.getRowIndex();
//					int cellIndex = tableLayout.getOrigin().getCellIndex() - 3;
//					tableLocation = new Location(rowIndex, cellIndex);
//				}
//
//				// if(tableLayout.getName()!=null ) {
//				// String className =tableLayout.getName();
//
//				// List<?> tableInstances =
//				// this.createInstances(Class.forName(className), tableRows);
//				// Mapper.setProperty(instance, className + "s",
//				// tableInstances);
//				// }
//				if (tableLayout.getName().equals("ruleDefinitionLayout")) {
//					List<RuleDefinition> tableInstances = this.createInstances(
//							RuleDefinition.class, tableRows);
//					Mapper.setProperty(instance, "ruleDefinitions",
//							tableInstances);
//
//				} else if (tableLayout.getName().equals("ruleConditionLayout")) {
//					List<RuleCondition> tableInstances = this.createInstances(
//							RuleCondition.class, tableRows);
//					Mapper.setProperty(instance, "ruleConditions",
//							tableInstances);
//				}
//
//				else if (tableLayout.getName().equals("conditionDataLayout")) {
//					// List<RuleCondition> tableInstances =
//					// this.createInstances(RuleCondition.class, tableRows);
//					Mapper.setProperty(instance, "conditionData", tableRows);
//				} else if (tableLayout.getName().equals("actionDataLayout")) {
//					// List<RuleCondition> tableInstances =
//					// this.createInstances(RuleCondition.class, tableRows);
//					Mapper.setProperty(instance, "actionData", tableRows);
//				}
//
//				else if (tableLayout.getName().equals("conditionTermLayout")) {
//					List<ConditionColumn> tableInstances = this
//							.createInstances(ConditionColumn.class, tableRows);
//					Mapper.setProperty(instance, "conditionColumns",
//							tableInstances);
//				}
//
//				else if (tableLayout.getName().equals("actionTermLayout")) {
//					List<ActionColumn> tableInstances = this.createInstances(
//							ActionColumn.class, tableRows);
//					Mapper.setProperty(instance, "actionColumns",
//							tableInstances);
//				}
//
//				System.out.print("");
//			}
//			// if(instance!=null) {
//			// result.add(instance);
//			// }
//			location = tableLocation;
//			location.setCellIndex(0);
//			layout.setOrigin(location);
//			// return readObjects(result,type, parentObject,location);
//
//			return instance;
//		}
//	}
//
//	private class BusinessRuleSheetReader extends SheetReader {
//		public BusinessRuleSheetReader(Sheet sheet, SheetLayout layout, Repository repository) {
//			super(sheet, layout,repository);
//			// TODO Auto-generated constructor stub
//		}
//
//		public BusinessRuleSheetReader(Sheet sheet, Repository repository) {
//			super(sheet, repository);
//		}
//
//		public <T> InstanceFactory getInstanceFactory(Class<T> type) {
//
//			if (type == BusinessRule.class) {
//				return new BusinessRuleFactory(this.getRepository());
//			}
//
//			else if (type == RuleDefinition.class) {
//				return new BusinessRuleFactory(this.getRepository());
//			}
//
//			else if (type == RuleCondition.class) {
//				return new BusinessRuleFactory(this.getRepository());
//			}
//
//			else if (type == RuleAction.class) {
//				return new BusinessRuleFactory(this.getRepository());
//			} else {
//				return new Factory<T>(this.getRepository());
//			}
//		}
//
//		public <T> List<T> readObjects(List<T> result, Class<T> type,
//				Object parentObject, Location location) throws Exception {
//
//			List<NameValuePair> keys = this.flattenKeys(parentObject);
//			ArrayList<ArrayList<NameValuePair>> rows = this.layout
//					.readLogicalRows(this.rowReader, keys);// , location);
//			ArrayList<NameValuePair> row = null;
//			if (rows != null && rows.size() > 0) {
//				row = rows.get(0);
//			}
//			if (row == null || row.size() == 0) {
//				return result;
//			}
//			T instance = this.createInstance(type, row);
//
//			List<TableLayout> tableLayouts = this.layout.getTableLayouts();
//			Orientation orientation = this.layout.getOrientation();
//			Location tableLocation = location;
//			if (orientation instanceof Horizontal) {
//				int rowIndex = layout.getOrigin().getRowIndex() + row.size()
//						+ 1;
//				int cellIndex = layout.getOrigin().getCellIndex() + 2;
//				tableLocation = new Location(rowIndex, cellIndex);
//
//			} else if (orientation instanceof Vertical) {
//				int rowIndex = layout.getOrigin().getRowIndex() + row.size()
//						+ 2;
//				int cellIndex = layout.getOrigin().getCellIndex();
//				tableLocation = new Location(rowIndex, cellIndex);
//
//			}
//
//			for (TableLayout tableLayout : tableLayouts) {
//
//				tableLayout.setOrigin(tableLocation);
//				// ArrayList<NameValuePair> k = new ArrayList<NameValuePair>();
//				ArrayList<ArrayList<NameValuePair>> tableRows = tableLayout
//						.readLogicalRows(this.rowReader);
//
//				Orientation tableOrientation = tableLayout.getOrientation();
//				if (tableOrientation instanceof Horizontal) {
//					int rowIndex = layout.getOrigin().getRowIndex()
//							+ row.size() + 1;
//					int cellIndex = layout.getOrigin().getCellIndex()
//							+ row.size() - 1;
//					tableLocation = new Location(rowIndex, cellIndex);
//
//				} else if (tableOrientation instanceof Vertical) {
//					int rowIndex = tableLayout.getOrigin().getRowIndex()
//							+ tableRows.size() + 2;
//					int cellIndex = tableLayout.getOrigin().getCellIndex();
//					tableLocation = new Location(rowIndex, cellIndex);
//				}
//
//				// if(tableLayout.getName()!=null ) {
//				// String className =tableLayout.getName();
//
//				// List<?> tableInstances =
//				// this.createInstances(Class.forName(className), tableRows);
//				// Mapper.setProperty(instance, className + "s",
//				// tableInstances);
//				// }
//				if (tableLayout.getName().equals("ruleDefinitionLayout")) {
//					List<RuleDefinition> tableInstances = this.createInstances(
//							RuleDefinition.class, tableRows);
//
//					Mapper.setProperty(instance, "ruleDefinitions",
//							tableInstances);
//				} else if (tableLayout.getName().equals("ruleConditionLayout")) {
//					List<RuleCondition> tableInstances = this.createInstances(
//							RuleCondition.class, tableRows);
//					Mapper.setProperty(instance, "ruleConditions",
//							tableInstances);
//				} else if (tableLayout.getName().equals("ruleActionLayout")) {
//					List<RuleAction> tableInstances = this.createInstances(
//							RuleAction.class, tableRows);
//					Mapper.setProperty(instance, "ruleActions", tableInstances);
//				}
//				System.out.print("");
//			}
//			if (instance != null) {
//				result.add(instance);
//			}
//			location = tableLocation;
//			location.setCellIndex(0);
//			layout.setOrigin(location);
//			return readObjects(result, type, parentObject, location);
//		}
//
//	}
//
//	public RuleProjectReader(InputStream workbookInputStream, Repository repository)
//			throws IOException {
//		super(workbookInputStream, repository);
//	}
//
//	@Override
//	public /*<T>*/ List/*<T>*/ readObjects(List/*<NameValuePair>*/ keys) throws Exception {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public /*<T>*/ List/*<T>*/ readObjects() throws Exception {
//		Sheet ruleProjectSheet = workBook.getSheet("RuleProject");
//		RuleProjectSheetReader reader = new RuleProjectSheetReader(
//				ruleProjectSheet, this.getRepository());
//		List<RuleProject> ruleProjects = reader.readObjects(RuleProject.class);
//		for (RuleProject ruleProject : ruleProjects) {
//			populateRuleProject(ruleProject);
//		}
//		return (List/*<T>*/) ruleProjects;
//	}
//
//	public <T> List<DecisionTable> readDecisionTable() throws Exception {
//		int numberOfSheets = workBook.getNumberOfSheets();
//		List<DecisionTable> result = new ArrayList<DecisionTable>();
//		
//		for (int sheetNum = 0; sheetNum < numberOfSheets; sheetNum++) {
//			Sheet decisionTableSheet = workBook.getSheetAt(sheetNum);
//			SheetLayout decisionTableLayout = getDecisionTableSheetLayout();
//			DecisionTableSheetReader decisionTableSheetReader = new DecisionTableSheetReader(
//					decisionTableSheet, decisionTableLayout,this.getRepository());
//
//			Location location = new Location(0, 0);
//			result.add(decisionTableSheetReader.readObject(DecisionTable.class,
//					location));
//
//		}
//		// Sheet DecisionTableSheet = workBook.getSheet("RuleProject");
//		// RuleProjectSheetReader reader = new
//		// RuleProjectSheetReader(DecisionTableSheet);
//		// List<RuleProject> ruleProjects =
//		// reader.readObjects(RuleProject.class);
//		// for (RuleProject ruleProject : ruleProjects) {
//		// populateRuleProject(ruleProject);
//		// }
//		return result;
//
//	}
//
//	private void populateRuleProject(RuleProject ruleProject) throws Exception {
//		
//		Sheet ruleSetParameterSheet = workBook.getSheet("RuleSetParameter");
//		RuleProjectSheetReader ruleSetParameterSheetReader= new RuleProjectSheetReader(ruleSetParameterSheet,this.getRepository());
//		List<RuleSetParameter> ruleSetParameterList=ruleSetParameterSheetReader.readObjects(RuleSetParameter.class, ruleProject);
//		ruleProject.setRuleSetParameters(ruleSetParameterList);
//		SheetLayout businessRuleSheetLayout = getBusinessRuleSheetLayout();
//		Sheet sheet = workBook.getSheet("BusinessRule - "
//				+ ruleProject.getRuleProjectName());
//		BusinessRuleSheetReader ruleSheetReader = new BusinessRuleSheetReader(
//				sheet, businessRuleSheetLayout,this.getRepository());
//		Location location = new Location(0, 0);
//		List<BusinessRule> result = new ArrayList<BusinessRule>();
//		ruleSheetReader.readObjects(result, BusinessRule.class, ruleProject,
//				location);
//		ruleProject.setRules(result);
//		
//		Sheet bomSheet = workBook.getSheet("BOM");
//		RuleProjectSheetReader bomSheetReader= new RuleProjectSheetReader(bomSheet,this.getRepository());
//		
//		List<BOM> bomList=bomSheetReader.readObjects(BOM.class, ruleProject);
//        for (BOM bom : bomList) {
//            Sheet bomClassSheet = workBook.getSheet("BOMClass");
//            RuleProjectSheetReader bomClassSheetReader= new RuleProjectSheetReader(bomClassSheet,this.getRepository());
//         
//            List<BOMClass> bomClasses = bomClassSheetReader.readObjects(BOMClass.class, bom);
//            bom.setBomClasses(bomClasses);
//            for (BOMClass bomClass : bomClasses) {
//            	Sheet bomElementSheet = workBook.getSheet("BOMElement");
//                RuleProjectSheetReader bomElementReader= new RuleProjectSheetReader(bomElementSheet,this.getRepository());
//              
//                List<BOMElement> bomElements = bomElementReader.readObjects(BOMElement.class, bomClass);
//                bomClass.setBomElements(bomElements);
//            }
//        }
//        ruleProject.setBom(bomList);
//	}
//
//	public static void main(String args[]) throws Exception {
//		System.out.print("");
////		RepositoryImpl repositoryImpl=new RepositoryImpl(classMapMapping, ePackage)
//		Mapper mapper=new Mapper(repository, lookupStrategy)
//		Repository fw2Repository=FW2Repository.getInstance();
//		FW2Repository.getInstance().getFW2Class(RuleProject.class);
//		FW2Repository.getInstance().getFW2Class(DecisionTable.class);
//		fw2Repository
//				.addClassMaps("C:\\Nevada\\SimplyExpress\\fw2\\RulesClassMaps.xlsx",fw2Repository);
//
//		InputStream mapFileStream = new FileInputStream("RuleProject.xlsx");
//		RuleProjectReader xlsxReader = new RuleProjectReader(mapFileStream,fw2Repository);
//		List<RuleProject> ruleProjects = xlsxReader.readObjects();
//		mapFileStream.close();
//		InputStream decisionTableFileStream = new FileInputStream(
//				"Decision Table Template.xlsx");
//		RuleProjectReader decisionTableXlsxReader = new RuleProjectReader(
//				decisionTableFileStream,fw2Repository);
//		List<DecisionTable> decisionTables = decisionTableXlsxReader
//				.readDecisionTable();
//		if (decisionTables != null) {
//			System.out.println();
//		}
//		for (RuleProject ruleProject : ruleProjects) {
//			RuleManager ruleManager = new RuleManager();
//			ruleManager.createProject(ruleProject);
//			ruleManager.createRuleProject(ruleProject);
//			List<BOM> bomList = ruleProject.getBom();
//			for (BOM bom : bomList) {
//				ruleManager.createBOM(ruleProject, bom);
//			}
//			List<BusinessRule> businessRules = ruleProject.getRules();
//			// for (BusinessRule businessRule : businessRules)
//			{
//				ruleManager.createBRL(ruleProject.getRepositoryLocation(),
//						ruleProject.getRuleProjectName(), businessRules);
//
//			}
//
//			if (decisionTables != null) {
//				for (DecisionTable decisionTable : decisionTables) {
//					if (ruleProject.getRuleProjectName().equalsIgnoreCase(
//							decisionTable.getRuleProjectName())) {
//						ruleManager.createDecisionTable(ruleProject,
//								decisionTable);
//					}
//				}
//			}
//			// DecisionTableXLSReader decisionTableXLSReader = new
//			// DecisionTableXLSReader();
//			// HashMap<String, List<DecisionTable>> decisionTableMap =
//			// decisionTableXLSReader.read("c:\\POC\\Decision Table Template.xlsx");
//			// List<DecisionTable> decisionTableList =
//			// decisionTableMap.get(ruleProject.getRuleProjectName());
//			// if (decisionTableList != null) {
//			// for (DecisionTable decisionTable : decisionTableList) {
//			// ruleManager.createDecisionTable(ruleProject, decisionTable);
//			// }
//			// }
//		}
//	}
//
//	private SheetLayout getBusinessRuleSheetLayout() {
//		SheetLayout businessRuleSheetLayout = new SheetLayout(
//				"businessRuleSheetLayout");
//		// businessRuleSheetLayout.setName("Horizontal");
//		SizeType sizeType = new VariableSize();
//		businessRuleSheetLayout.setSizeType(sizeType);
//		Orientation orientation = new Horizontal();
//		businessRuleSheetLayout.setOrientation(orientation);
//		businessRuleSheetLayout.setOrigin(new Location(0, 0));
//		TableLayout ruleDefintiionLayout = new TableLayout(
//				"ruleDefinitionLayout");
//		// ruleDefintiionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		ruleDefintiionLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		ruleDefintiionLayout.setOrientation(orientation);
//		ruleDefintiionLayout.setOrigin(new Location(0, 0));
//		TableLayout ruleConditionLayout = new TableLayout("ruleConditionLayout");
//		// ruleConditionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		ruleConditionLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		ruleConditionLayout.setOrientation(orientation);
//		ruleConditionLayout.setOrigin(new Location(0, 0));
//		TableLayout ruleActionLayout = new TableLayout("ruleActionLayout");
//		// ruleActionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		ruleActionLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		ruleActionLayout.setOrientation(orientation);
//		ruleActionLayout.setOrigin(new Location(0, 0));
//		businessRuleSheetLayout.getTableLayouts().add(ruleDefintiionLayout);
//		businessRuleSheetLayout.getTableLayouts().add(ruleConditionLayout);
//		businessRuleSheetLayout.getTableLayouts().add(ruleActionLayout);
//		return businessRuleSheetLayout;
//	}
//
//	private SheetLayout getDecisionTableSheetLayout() {
//		SheetLayout businessRuleSheetLayout = new SheetLayout(
//				"decisionTableSheetLayout");
//		// businessRuleSheetLayout.setName("Horizontal");
//		SizeType sizeType = new VariableSize();
//		businessRuleSheetLayout.setSizeType(sizeType);
//		Orientation orientation = new Horizontal();
//		businessRuleSheetLayout.setOrientation(orientation);
//		businessRuleSheetLayout.setOrigin(new Location(0, 0));
//		TableLayout ruleDefintiionLayout = new TableLayout(
//				"ruleDefinitionLayout");
//		// ruleDefintiionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		ruleDefintiionLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		ruleDefintiionLayout.setOrientation(orientation);
//		ruleDefintiionLayout.setOrigin(new Location(0, 0));
//		TableLayout ruleConditionLayout = new TableLayout("ruleConditionLayout");
//		// ruleConditionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		ruleConditionLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		ruleConditionLayout.setOrientation(orientation);
//		ruleConditionLayout.setOrigin(new Location(0, 0));
//
//		TableLayout conditionTermLayout = new TableLayout("conditionTermLayout");
//		// ruleConditionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		conditionTermLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		conditionTermLayout.setOrientation(orientation);
//		conditionTermLayout.setOrigin(new Location(0, 0));
//
//		TableLayout conditionDataLayout = new TableLayout("conditionDataLayout");
//		// ruleConditionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		conditionDataLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		conditionDataLayout.setOrientation(orientation);
//		conditionDataLayout.setOrigin(new Location(0, 0));
//
//		TableLayout actionTermLayout = new TableLayout("actionTermLayout");
//		// ruleConditionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		actionTermLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		actionTermLayout.setOrientation(orientation);
//		actionTermLayout.setOrigin(new Location(0, 0));
//
//		TableLayout actionDataLayout = new TableLayout("actionDataLayout");
//		// ruleConditionLayout.setName("Vertical");
//		sizeType = new VariableSize();
//		actionDataLayout.setSizeType(sizeType);
//		orientation = new Vertical();
//		actionDataLayout.setOrientation(orientation);
//		actionDataLayout.setOrigin(new Location(0, 0));
//
//		businessRuleSheetLayout.getTableLayouts().add(ruleDefintiionLayout);
//		businessRuleSheetLayout.getTableLayouts().add(ruleConditionLayout);
//		businessRuleSheetLayout.getTableLayouts().add(conditionTermLayout);
//		businessRuleSheetLayout.getTableLayouts().add(conditionDataLayout);
//		businessRuleSheetLayout.getTableLayouts().add(actionTermLayout);
//		businessRuleSheetLayout.getTableLayouts().add(actionDataLayout);
//		return businessRuleSheetLayout;
//	}
}
