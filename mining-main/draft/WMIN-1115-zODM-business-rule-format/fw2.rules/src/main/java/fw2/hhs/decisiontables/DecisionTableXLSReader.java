package fw2.hhs.decisiontables;
//
//import java.io.FileInputStream;
//import java.io.FileNotFoundException;
//import java.io.IOException;
//import java.io.InputStream;
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.List;
//
//import org.apache.poi.poifs.filesystem.POIFSFileSystem;
//import org.apache.poi.ss.usermodel.Cell;
//import org.apache.poi.xssf.usermodel.XSSFRow;
//import org.apache.poi.xssf.usermodel.XSSFSheet;
//import org.apache.poi.xssf.usermodel.XSSFWorkbook;
//
//import fw2.hhs.rules.RuleCondition;
//import fw2.hhs.rules.RuleDefinition;
//import fw2.hhs.rules.RuleProject;
//
public class DecisionTableXLSReader {
//
//	public static void main(String args[]) {
//		DecisionTableXLSReader decisionTableXLSReader = new DecisionTableXLSReader();
//		decisionTableXLSReader.read("c:\\POC\\Decision Table Template.xlsx");
//
//	}
//
//	public HashMap<String, List<DecisionTable>> read(String decisionTableXLS) {
//
//		InputStream dtFileStream = null;
//		HashMap<String, List<DecisionTable>> decisionTableMap=new HashMap<String, List<DecisionTable>>();
//		List<DecisionTable> decisionTables = new ArrayList<DecisionTable>();
//		try {
//			dtFileStream = new FileInputStream(decisionTableXLS);
//		} catch (FileNotFoundException e) {
//			System.out.println("File not found in the specified path.");
//			e.printStackTrace();
//			return decisionTableMap;
//		}
//
//		try {
//			// POIFSFileSystem fileSystem = new POIFSFileSystem(dtFileStream);
//			XSSFWorkbook workBook = new XSSFWorkbook(dtFileStream);
//			XSSFSheet decisionTableNameWorkSheet = workBook
//					.getSheet("DecisionTableList");
//
//			int lastRowNum = decisionTableNameWorkSheet.getLastRowNum();
//
//			for (int i = 1; i <= lastRowNum; i++) {
//				XSSFRow row = decisionTableNameWorkSheet.getRow(i);
//				String ruleProjectName = row.getCell(0).getStringCellValue();
//				String rulePackage = row.getCell(1).getStringCellValue();
//				String decisionTableName = row.getCell(2).getStringCellValue();
//				DecisionTable decisionTable = new DecisionTable();
//				decisionTable.setRulePackage(rulePackage);
//				decisionTable.setDecisionTableName(decisionTableName);
//				List<DecisionTable> decisionTableInRuleProject=decisionTableMap.get(ruleProjectName);
//				if(decisionTableInRuleProject==null) {
//					decisionTableInRuleProject=new ArrayList<DecisionTable>();
//				}
//				decisionTables.add(decisionTable);
//				decisionTableInRuleProject.add(decisionTable);
//				decisionTableMap.put(ruleProjectName, decisionTableInRuleProject);
//
//			}
//
//			for (DecisionTable decisionTable : decisionTables) {
//				XSSFSheet decisionTableWorkSheet = workBook
//						.getSheet(decisionTable.getDecisionTableName());
//				System.out.println("decisionTable: " +decisionTable.getDecisionTableName());
//				int lastRowNumInDTSheet = decisionTableWorkSheet
//						.getLastRowNum();
//				for (int i = 0; i <= lastRowNumInDTSheet; i++) {
//					XSSFRow decisionTableSheetRow = decisionTableWorkSheet
//							.getRow(i);
//					System.out.println("decisionTableSheetRowNumber: " +i);
//					if (decisionTableSheetRow.getPhysicalNumberOfCells() > 0) {
//						String columnOne = decisionTableSheetRow.getCell(0)
//								.getStringCellValue();
//
//						if (columnOne.equalsIgnoreCase("DOCUMENTATION")) {
//							String documentation = decisionTableSheetRow
//									.getCell(1).getStringCellValue();
//							decisionTable.setDocumentation(documentation);
//						}
//
//						if (columnOne.equalsIgnoreCase("DEFINITIONS")) {
//							int j = i + 1;
//							XSSFRow headerRow = decisionTableSheetRow;
//							i++;
//							decisionTableSheetRow = decisionTableWorkSheet
//									.getRow(i);
//							while (columnOne.equalsIgnoreCase("PRECONDITIONS") == false
//									&& j <= lastRowNumInDTSheet) {
//								String ruleDefinitionTerm = "";
//								String ruleDefinitionType = "";
//								String ruleDefinitionCollectionReference = "";
//								String ruleDefinitionReferenceElement = "";
//								String ruleDefinitionReferenceHolder= "";
//
//								for (Cell cell : decisionTableSheetRow) {
//									int index = cell.getColumnIndex();
//									switch (index) {
//									case 1:
//										ruleDefinitionTerm = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 2:
//										ruleDefinitionType = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 3:
//										ruleDefinitionCollectionReference = readCell(decisionTableSheetRow
//												.getCell(index));
//									case 4:
//										ruleDefinitionReferenceElement = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 5:
//										ruleDefinitionReferenceHolder = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//
//									}
//								}
//
//								if (ruleDefinitionTerm.trim().length() > 0) {
//									RuleDefinition ruleDefinition = new RuleDefinition();
//									ruleDefinition.setTerm(ruleDefinitionTerm);
//									ruleDefinition
//											.setDefinitionType(ruleDefinitionType);
//									/*ruleDefinition
//											.setCollectionReferenceTerm(ruleDefinitionCollectionReference);
//							*/		ruleDefinition
//											.setReferenceElementTerm(ruleDefinitionReferenceElement);
//									ruleDefinition
//											.setReferenceHolderTerm(ruleDefinitionReferenceHolder);
//									decisionTable.getRuleDefinitions().add(
//											ruleDefinition);
//								}
//
//								j++;
//								i = j;
//								decisionTableSheetRow = decisionTableWorkSheet
//										.getRow(j);
//								if (decisionTableSheetRow != null) {
//									if(decisionTableSheetRow.getCell(0)!=null) {
//										columnOne = readCell(decisionTableSheetRow.getCell(0));
//									}
//								}
//
//							}
//						}
//						if (columnOne.equalsIgnoreCase("PRECONDITIONS")) {
//							int j = i + 1;
//							XSSFRow headerRow = decisionTableSheetRow;
//							i++;
//							decisionTableSheetRow = decisionTableWorkSheet
//									.getRow(i);
//							while (columnOne.equalsIgnoreCase("CONDITIONS") == false
//									&& j <= lastRowNumInDTSheet) {
//								
//								String ruleConditionTerm = "";
//								String ruleConditionReferenceTerm = "";
//								String ruleConditionalOperator = "";
//								String ruleConditionValue = "";
//								String ruleConcatinationOperator = "";
//
//								for (Cell cell : decisionTableSheetRow) {
//									int index = cell.getColumnIndex();
//									switch (index) {
//									case 1:
//										ruleConditionTerm = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 2:
//										ruleConditionReferenceTerm = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 3:
//										ruleConditionalOperator = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 4:
//										ruleConditionValue = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//									case 5:
//										ruleConcatinationOperator = readCell(decisionTableSheetRow
//												.getCell(index));
//										break;
//
//									}
//								}
//								if(ruleConditionTerm.trim().length()>0) {
//								RuleCondition ruleCondition = new RuleCondition();
//
//								ruleCondition.setTerm(ruleConditionTerm);
//								ruleCondition
//										.setReferenceTerm(ruleConditionReferenceTerm);
//								ruleCondition
//										.setConditionalOperator(ruleConditionalOperator);
//								ruleCondition.setValue(ruleConditionValue);
//								ruleCondition
//										.setConcatinationOperator(ruleConcatinationOperator);
//
//								
//								decisionTable.getRuleConditions().add(
//										ruleCondition);
//								}
//
//								j++;
//								i = j;
//								decisionTableSheetRow = decisionTableWorkSheet
//										.getRow(j);
//								if (decisionTableSheetRow != null) {
//									if(decisionTableSheetRow.getCell(0)!=null) {
//										columnOne = readCell(decisionTableSheetRow.getCell(0));
//									}
//								}
//
//							}
//						}
//
//
//						if (columnOne.equalsIgnoreCase("CONDITIONS")) {
//							int j = 0;
//
//							XSSFRow headerRow = decisionTableSheetRow;
//							i++;
//							decisionTableSheetRow = decisionTableWorkSheet
//									.getRow(i);
//							columnOne = decisionTableSheetRow.getCell(0)
//									.getStringCellValue();
//							while (columnOne.equalsIgnoreCase("ACTIONS") == false
//									&& i <= lastRowNumInDTSheet) {
//								String columnName = "";
//								String columnTerm = "";
//								List<List<String>> rowData = decisionTable
//										.getConditionData();
//
//								for (Cell cell : decisionTableSheetRow) {
//									int index = cell.getColumnIndex();
//									switch (index) {
//									case 0:
//										columnName = readCell(cell);
//										break;
//									case 1:
//										columnTerm = readCell(cell);
//										break;
//									default:
//										int rowNum = index - 2;
//										if (rowData.size() <= rowNum) {
//											rowData.add(new ArrayList<String>());
//										}
//										List<String> row = rowData.get(rowNum);
//										if (row.size() <= j) {
//											row.add("");
//										}
//										row.set(j, readCell(cell));
//										rowData.set(rowNum, row);
//
//									}
//								}
//
//								DecisionTableColumn decisionTableColumn = new DecisionTableColumn();
//								decisionTableColumn.setColumnName(columnName);
//								decisionTableColumn.setColumnTerm(columnTerm);
//
//								decisionTable.getConditionColumns().add(
//										decisionTableColumn);
//
//								// int index=0;
//								// for (Cell cell : headerRow) {
//								//
//								//
//								//
//								//
//								//
//								// }
//
//								j++;
//								i++;
//								decisionTableSheetRow = decisionTableWorkSheet
//										.getRow(i);
//								if (decisionTableSheetRow != null) {
//									if(decisionTableSheetRow.getCell(0)!=null) {
//										columnOne = readCell(decisionTableSheetRow.getCell(0));
//									}
//								}
//
//							}
//						}
//
//						if (columnOne.equalsIgnoreCase("ACTIONS")) {
//							int j = 0;
//
//							XSSFRow headerRow = decisionTableSheetRow;
//							i++;
//							decisionTableSheetRow = decisionTableWorkSheet
//									.getRow(i);
//							columnOne = decisionTableSheetRow.getCell(0)
//									.getStringCellValue();
//							while (i <= lastRowNumInDTSheet) {
//								String columnName = "";
//								String columnTerm = "";
//								List<List<String>> rowData = decisionTable
//										.getActionData();
//
//								for (Cell cell : decisionTableSheetRow) {
//									int index = cell.getColumnIndex();
//									switch (index) {
//									case 0:
//										columnName = readCell(cell);
//										break;
//									case 1:
//										columnTerm = readCell(cell);
//										break;
//									default:
//										int rowNum = index - 2;
//										if (rowData.size() <= rowNum) {
//											rowData.add(new ArrayList<String>());
//										}
//										List<String> row = rowData.get(rowNum);
//										if (row.size() <= j) {
//											row.add("");
//										}
//										row.set(j, readCell(cell));
//										rowData.set(rowNum, row);
//
//									}
//								}
//
//								DecisionTableColumn decisionTableColumn = new DecisionTableColumn();
//								decisionTableColumn.setColumnName(columnName);
//								decisionTableColumn.setColumnTerm(columnTerm);
//
//								decisionTable.getActionColumns().add(
//										decisionTableColumn);
//
//								// int index=0;
//								// for (Cell cell : headerRow) {
//								//
//								//
//								//
//								//
//								//
//								// }
//
//								j++;
//								i++;
//								if (i <= lastRowNumInDTSheet) {
//									decisionTableSheetRow = decisionTableWorkSheet
//											.getRow(i);
//									System.out.println("i : " +i);
//									if (decisionTableSheetRow != null) {
//										
//										columnOne = decisionTableSheetRow
//												.getCell(0)
//												.getStringCellValue();
//									}
//								}
//
//							}
//						}
//
//					}
//					System.out.print("END");
//				}
//			}
//			System.out.print("END");
//
//		} catch (FileNotFoundException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		return decisionTableMap;
//
//	}
//
//	String readCell(Cell cell) {
//		int cellType = cell.getCellType();
//		String cellValue = "";
//
//		switch (cellType) {
//		case Cell.CELL_TYPE_STRING:
//			cellValue = cell.getStringCellValue();
//			break;
//		case Cell.CELL_TYPE_NUMERIC:
//			int value = (int) cell.getNumericCellValue();
//			cellValue = String.valueOf(value);
//			break;
//		case Cell.CELL_TYPE_BOOLEAN:
//			boolean booleanValue = (boolean) cell.getBooleanCellValue();
//			cellValue = String.valueOf(booleanValue);
//			break;
//		}
//		return cellValue;
//	}
}
