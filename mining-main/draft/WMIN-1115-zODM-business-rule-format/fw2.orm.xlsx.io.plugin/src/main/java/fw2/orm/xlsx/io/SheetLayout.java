package fw2.orm.xlsx.io;

public class SheetLayout extends LayoutContainer {
	public static SheetLayout defaultLayout = null;

	public static SheetLayout getDefaultLayout() {

		if (SheetLayout.defaultLayout == null) {

			SheetLayout.defaultLayout = new SheetLayout("defaultSheetLayout");

			SheetLayout.defaultLayout.setSizeType(new VariableSize());
			SheetLayout.defaultLayout.setOrientation(new Vertical());
			SheetLayout.defaultLayout.setOrigin(new Location(0, 0));

		}
		return SheetLayout.defaultLayout;
	}

	public SheetLayout(final String name) {
		super(name);
	}

}
