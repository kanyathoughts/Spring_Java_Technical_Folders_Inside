/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.ui;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * This class is simply used to simulate a Fieldeditor with only a text field.
 * It is used to show the deep links port in the mining preferences.
 */
public class DeepLinkFieldEditor extends FieldEditor {
	private Composite top;
	
	/**
	 * Constructor of the Field editor
	 * 
	 * @param name Name of the property managed by this editor
	 * @param labelText Text shown in the label
	 * @param parent The parent which will contain this editor
	 */
	public DeepLinkFieldEditor(final String name, final String labelText, final Composite parent) {
		super(name, labelText, parent);
	}

	@Override
	protected void adjustForNumColumns(final int numColumns) {
		((GridData)top.getLayoutData()).horizontalSpan = numColumns;
	}

	@Override
	protected void doFillIntoGrid(final Composite parent, final int numColumns) {
		top = parent;

		final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = numColumns;
		top.setLayoutData(gd);

		final Label label = getLabelControl(top);
		final GridData labelData = new GridData();
		labelData.horizontalSpan = numColumns;
		label.setLayoutData(labelData);
		
	}

	@Override
	protected void doLoad() {
		final int port = getPreferenceStore().getInt(MiningPreferences.KEY_DEEP_LINKS_PORT);
		setPort(port);
	}
	
	private void setPort(final int port) {
		if (port > 0) {
			setLabelText("The Deep Links server (\"Open in Eclipse\") is listening on port " + port);
		} else {
			setLabelText("The Deep Links server is not running.");
		}
	}

	@Override
	protected void doLoadDefault() {
		final int port = getPreferenceStore().getDefaultInt(MiningPreferences.KEY_DEEP_LINKS_PORT);
		setPort(port);
	}

	@Override
	protected void doStore() {
		/* Since this editor doesn't allow for input, there is no need for storing values */
	}

	@Override
	public int getNumberOfControls() {
		return 2;
	}

}
