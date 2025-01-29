/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.view;

import innowake.mining.plugin.ui.WebBasedEditorView;
import innowake.mining.shared.entities.DataDictionaryPojo;


/**
 * Web-based {@link DataDictionaryPojo} editor view.
 */
public class DataDictionaryEditorView extends WebBasedEditorView<DataDictionaryPojo> {

	/** The ID of this view. */
	public static final String ID = "innowake.mining.dataDictionaryEntry.view.dataDictionaryEditor";
	
	private static final String CREATE_URL = "#/project-%s/module-%s/data-dictionary-entry-editor?offset=%s&length=%s";
	private static final String UPDATE_URL = "#/project-%s/module-%s/data-dictionary-entry-editor/%d";
	
	@Override
	protected
	String getCreateUrlPattern() {
		return CREATE_URL;
	}
	
	@Override
	protected
	String getUpdateUrlPattern() {
		return UPDATE_URL;
	}

	@Override
	protected String getViewId() {
		return ID;
	}
}
