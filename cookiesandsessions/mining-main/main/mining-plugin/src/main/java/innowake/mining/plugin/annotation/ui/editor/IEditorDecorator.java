/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.source.ISourceViewerExtension5;

import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * A decorator which gets registered on source viewers. 
 */
public interface IEditorDecorator {

	/**
	 * Signifies if the decorator is active or not.
	 * <p>
	 * If the decorator is not active then it will not be installed via {@link #install(ISourceViewerExtension5)}.
	 *
	 * @return {@code true} if the decorator is active, {@code false} otherwise
	 */
	default boolean isActive() {
		final Optional<IFile> file = EditorActionUtil.getFile();
		return file.isPresent() && MiningPreferences.getApiProject(file.get().getProject()).isPresent();
	}

	/**
	 * Installs the decorator given the source viewer extension.
	 * <p>
	 * This is only called when {@link #isActive()} returns {@code true}.
	 *
	 * @param sourceViewerExtension the source viewer extension
	 */
	void install(ISourceViewerExtension5 sourceViewerExtension);

	/**
	 * Uninstalls the decorator.
	 */
	void uninstall();

}
