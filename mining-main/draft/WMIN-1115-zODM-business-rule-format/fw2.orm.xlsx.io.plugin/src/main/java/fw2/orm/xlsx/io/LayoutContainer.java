package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

public class LayoutContainer extends TableLayout {
	List<TableLayout> tableLayouts = new ArrayList<TableLayout>();

	public LayoutContainer(final String name) {
		super(name);
	}

	public void addTableLayout(final TableLayout tableLayout) {
		this.tableLayouts.add(tableLayout);
		tableLayout.setContainer(this);
	}

	public List<TableLayout> getTableLayouts() {
		return this.tableLayouts;
	}

	public void setTableLayouts(final List<TableLayout> tableLayouts) {
		for (final TableLayout layout : tableLayouts) {
			this.addTableLayout(layout);
		}
	}

}
