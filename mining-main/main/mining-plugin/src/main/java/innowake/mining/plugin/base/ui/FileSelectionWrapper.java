/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.Optional;

import org.eclipse.core.resources.IFile;

/**
 * Wraps a {@link Optional} of {@link IFile} in a structured selection.
 */
public class FileSelectionWrapper extends StructuredSelectionAdapter {

	private final Optional<IFile> file;

	/**
	 * Create the new instance. 
	 * @param file The optional file instance to wrap.
	 */
	public FileSelectionWrapper(final Optional<IFile> file) {
		this.file = file;
	}
	
	/**
	 * Returns {@code true} if optional file is not set.
	 * 
	 * @return {@code true} if optional file is not set
	 */
	@Override
	public boolean isEmpty() {
		return ! file.isPresent();
	}

	/**
	 * Returns the file.
	 * 
	 * @return the file
	 * @throws IllegalStateException if optional file is not set
	 */
	@Override
	public Object getFirstElement() {
		return file.orElseThrow(IllegalStateException::new);
	}
	
}
