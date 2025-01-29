/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.model.export;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import com.google.common.io.Files;
import innowake.mining.plugin.fieldtracer.view.FieldtracerView;
import innowake.ndt.fieldtracing.model.FieldDefinition;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.export.ExportFormat;

/**
 * Used to store a field trace export as file in the workspace.
 * <br>The project relative subfolder is set to <code>/fieldtraces</code>
 * <br>Filename is set by the pattern <code>modulename_fieldname.csv</code>
 * <br>
 * @param <O> {@link IResource} type of program object 
 */
public class IFileStorage<O extends IResource> {

	private static final String PROJECT_FOLDER = "fieldtraces";
	
	/**
	 * Checks if the file exists
	 * 
	 * @param exportFormat of the file export
	 * @param model the of the exported trace
	 * @return {@code true} if the file exists, {@code false} otherwise
	 * @throws CoreException in case of an Error occurring
	 */
	public boolean exist(final ExportFormat exportFormat, final Model<O> model) throws CoreException {
		final IProject project = model.getSelectedField().getModule().getSourceObject().getProject();
		final IFolder targetFolder = ensureFolder(project, PROJECT_FOLDER);
		final IFile targetFile = targetFolder.getFile(createExportFileName(exportFormat, model));
		return targetFile.exists();
	}
	
	/**
	 * Stores an export to a file
	 * 
	 * @param exportFormat of the file export
	 * @param model the of the exported trace
	 * @param override {@code true} to override an file in case it exists
	 * @return {@code true} if the file exists, {@code false} otherwise
	 * @throws CoreException in case of an Error occurring
	 * @throws IOException in case of an Error occurring
	 */
	public IFile store(final ExportFormat exportFormat, final Model<O> model, final boolean override) throws CoreException, IOException {
		final IProject project = model.getSelectedField().getModule().getSourceObject().getProject();
		final IFolder targetFolder = ensureFolder(project, PROJECT_FOLDER);
		final IFile targetFile = targetFolder.getFile(createExportFileName(exportFormat, model));
		
		if ( ! override && targetFile.exists()) {
			throw new CoreException(new Status(IStatus.ERROR, FieldtracerView.ID, "File " + targetFile.getName() + " already exist."));
		} else if (targetFile.exists()) {
			try (ByteArrayInputStream source = new ByteArrayInputStream(exportFormat.export(model).getBytes("UTF-8"))) {
				targetFile.setContents( source, IResource.FORCE & IResource.KEEP_HISTORY, null);
			}
		} else {
			try (ByteArrayInputStream source = new ByteArrayInputStream(exportFormat.export(model).getBytes("UTF-8"))) {
				targetFile.create(source, true, null);
			}
		}
		return targetFile;
	}
	
	private String createExportFileName(final ExportFormat exportFormat, final Model<O> model) {
		final FieldDefinition<O> selectedField = model.getSelectedField();
		return String.format("%s_%s.%s", 
				Files.getNameWithoutExtension(selectedField.getModule().getName()),
				selectedField.getFieldName(),
				exportFormat.getFileExtension());
	}
	
	private IFolder ensureFolder(final IProject project, final String folderName) throws CoreException {
		final IFolder folder = project.getFolder(folderName);
		if ( ! folder.exists()) {
			folder.create(true, true, null);
			if ( ! folder.exists()) {
				throw new CoreException(new Status(IStatus.ERROR, FieldtracerView.ID, "Unable to create export folder."));
			}
		}
		return folder;
	}
	
}
