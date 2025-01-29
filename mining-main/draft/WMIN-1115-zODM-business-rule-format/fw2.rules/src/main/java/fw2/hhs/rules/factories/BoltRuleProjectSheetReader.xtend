package fw2.hhs.rules.factories

import fw2.hhs.rules.RuleAction
import fw2.hhs.rules.RuleCondition
import fw2.hhs.rules.RuleDefinition
import fw2.orm.xlsx.io.Factory
import fw2.orm.xlsx.io.Horizontal
import fw2.orm.xlsx.io.InstanceFactory
import fw2.orm.xlsx.io.Location
import fw2.orm.xlsx.io.NameValuePair
import fw2.orm.xlsx.io.Orientation
import fw2.orm.xlsx.io.SheetLayout
import fw2.orm.xlsx.io.SheetReader
import fw2.orm.xlsx.io.TableLayout
import fw2.orm.xlsx.io.Vertical
import fw2.orm.xlsx.io.mapper.Mapper
import java.util.ArrayList
import java.util.List
import org.apache.poi.ss.usermodel.Sheet

package class BoltRuleProjectSheetReader extends SheetReader {
	/** 
	 */
	final BoltRuleProjectXlsxReader boltProjectXlsxReader
	InstanceFactory factory

	new(BoltRuleProjectXlsxReader boltProjectXlsxReader, Sheet sheet, Mapper mapper) {
		super(sheet, mapper)
		this.boltProjectXlsxReader = boltProjectXlsxReader
	}

	new(BoltRuleProjectXlsxReader boltProjectXlsxReader, Sheet sheet, SheetLayout layout, Mapper mapper) {
		super(sheet, layout, mapper)
		this.boltProjectXlsxReader = boltProjectXlsxReader
	}

	override <T> InstanceFactory getInstanceFactory(Class<T> type) {
		if (this.factory === null) {
			this.factory = new Factory(this.boltProjectXlsxReader.getMapper())
		}
		return this.factory
	}

	def <T> List<T> readObjects(List<T> result, Class<T> type, Object parentObject,
		Location location) throws Exception {

		val List<NameValuePair> keys = this.mapper.flattenKeys(parentObject);
		this.layout.orientation = new Horizontal();
		val ArrayList<ArrayList<NameValuePair>> rows = this.layout.readLogicalRows(this.rowReader, keys); // , location);
		var ArrayList<NameValuePair> row = null;
		if (rows !== null && rows.size() > 0) {
			row = rows.get(0);
		}
		if (row === null || row.size() == 0) {
			return result;
		}
		val T instance = this.createInstance(type, row);

		var Location tableLocation = location;
		val Orientation orientation = this.layout.getOrientation();
		
		if (orientation instanceof Horizontal) {
			var int rowIndex = layout.getOrigin().getRowIndex() + row.size() + 1;
			var int cellIndex = layout.getOrigin().getCellIndex() + 2;
			tableLocation = new Location(rowIndex, cellIndex);

		} else if (orientation instanceof Vertical) {
			var int rowIndex = layout.getOrigin().getRowIndex() + row.size() + 2;
			var int cellIndex = layout.getOrigin().getCellIndex();
			tableLocation = new Location(rowIndex, cellIndex);

		}
		this.layout.orientation = new Vertical
		
		val List<TableLayout> tableLayouts = this.layout.getTableLayouts();


		for (TableLayout tableLayout : tableLayouts) {

			tableLayout.setOrigin(tableLocation);
			// ArrayList<NameValuePair> k = new ArrayList<NameValuePair>();
			val ArrayList<ArrayList<NameValuePair>> tableRows = tableLayout.readLogicalRows(this.rowReader);

			val Orientation tableOrientation = tableLayout.getOrientation();
			if (tableOrientation instanceof Horizontal) {
				var int rowIndex = layout.getOrigin().getRowIndex() + row.size() + 1;
				var int cellIndex = layout.getOrigin().getCellIndex() + row.size() - 1;
				tableLocation = new Location(rowIndex, cellIndex);

			} else if (tableOrientation instanceof Vertical) {
				var int rowIndex = tableLayout.getOrigin().getRowIndex() + tableRows.size() + 2;
				var int cellIndex = tableLayout.getOrigin().getCellIndex();
				tableLocation = new Location(rowIndex, cellIndex);
			}

			// if(tableLayout.getName()!=null ) {
			// String className =tableLayout.getName();
			// List<?> tableInstances =
			// this.createInstances(Class.forName(className), tableRows);
			// Mapper.setProperty(instance, className + "s",
			// tableInstances);
			// }
			if (tableLayout.getName().equals("ruleDefinitionLayout")) {
				val List<RuleDefinition> tableInstances = this.createInstances(RuleDefinition, tableRows);
				this.mapper.setProperty(instance, "ruleDefinitions", tableInstances);
			} else if (tableLayout.getName().equals("ruleConditionLayout")) {
				val List<RuleCondition> tableInstances = this.createInstances(RuleCondition, tableRows);
				this.mapper.setProperty(instance, "ruleConditions", tableInstances);
			} else if (tableLayout.getName().equals("ruleActionLayout")) {
				val List<RuleAction> tableInstances = this.createInstances(RuleAction, tableRows);
				this.mapper.setProperty(instance, "ruleActions", tableInstances);
			}
			System.out.print("");
		}
		if (instance != null) {
			result.add(instance);
		}
//			location = tableLocation;
		location.rowIndex = tableLocation.rowIndex
		location.setCellIndex(0);
		layout.setOrigin(location);
		return readObjects(result, type, parentObject, location);
	}

}
