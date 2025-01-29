/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.annotation.ui.view;

import innowake.mining.plugin.ui.WebBasedEditorView;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Web-based {@link AnnotationPojo} editor view. 
 */
public class AnnotationEditorView extends WebBasedEditorView<AnnotationPojo> {

	/** The ID of this view. */
	public static final String ID = "innowake.mining.annotation.view.annotationEditor";

	private static final String CREATE_URL = "#/project-%s/module-%s/annotation-editor?offset=%s&length=%s";
	private static final String UPDATE_URL = "#/project-%s/module-%s/annotation-editor/%d";
	
	@Override
	protected String getUpdateUrlPattern() {
		return UPDATE_URL;
	}

	@Override
	protected String getCreateUrlPattern() {
		return CREATE_URL;
	}

	@Override
	protected String getViewId() {
		return ID;
	}

}
