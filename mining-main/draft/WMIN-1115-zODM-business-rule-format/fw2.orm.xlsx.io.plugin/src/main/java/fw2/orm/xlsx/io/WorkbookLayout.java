package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class WorkbookLayout extends LayoutContainer {
	public static WorkbookLayout defaultLayout = null;

	public static WorkbookLayout getDefaultLayout() {
		if (WorkbookLayout.defaultLayout == null) {
			WorkbookLayout.defaultLayout = new WorkbookLayout("defaultWorkbookLayout");
			WorkbookLayout.defaultLayout.setSizeType(new VariableSize());
			WorkbookLayout.defaultLayout.setOrientation(new Vertical());
			WorkbookLayout.defaultLayout.setOrigin(new Location(0, 0));
		}
		return WorkbookLayout.defaultLayout;
	}

	Map<String, SheetLayout> sheetLayouts = new HashMap<String, SheetLayout>();

	public WorkbookLayout(final String name) {
		super(name);
	}

	public void addSheetLayout(final SheetLayout sheetLayout) {
		this.sheetLayouts.put(sheetLayout.getName(), sheetLayout);
		sheetLayout.setContainer(this);

	}

	public SheetLayout getSheetLayout(final String sheetName) {
		return this.sheetLayouts.get(sheetName);
	}

	public List<SheetLayout> getSheetLayouts() {
		final ArrayList<SheetLayout> arrayList = new ArrayList<SheetLayout>();
		arrayList.addAll(this.sheetLayouts.values());
		return arrayList;
	}

	public void setSheetLayouts(final List<SheetLayout> layouts) {
		for (final SheetLayout layout : layouts) {
			this.addSheetLayout(layout);
		}
	}

}
