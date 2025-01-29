/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;

/**
 * Utility methods for {@link IResource}.
 */
public final class ResourceUtil {

	private ResourceUtil() {}

	/**
	 * Returns all containing {@linkplain IFile files} for an {@link IResource} recursively.
	 *
	 * @param resource the {@link IResource}
	 * @return all containing {@linkplain IFile files} recursively
	 * @throws CoreException if error occurs
	 */
	public static List<IFile> getFilesRecursively(final IResource resource) throws CoreException {
		final List<IFile> result = new ArrayList<>();
		if (resource instanceof IContainer) {
			for (final IResource member : ((IContainer) resource).members()) {
				result.addAll(getFilesRecursively(member));
			}
		} else if (resource instanceof IFile) {
			result.add((IFile) resource);
		}
		return result;
	}
	
	/**
	 * Returns a path as string for a {@link IFile} relative to it's project.
	 *
	 * @param file the file
	 * @return the path of the file relative to it's project
	 */
	public static String getProjectRelativePath(final IFile file) {
		final IPath projectPath = file.getProject().getFullPath();
		final IPath filePath = file.getFullPath();
		return filePath.makeRelativeTo(projectPath).toString();
	}
	
	/**
	 * Return the fileInfo at pathName or <code>null</code> if the format is
	 * invalid or if the file info cannot be determined.
	 * Copied from {@code org.eclipse.ui.internal.ide.dialogs.IDEResourceInfoUtils}
	 *
	 * @param pathName the Resource path
	 * @return IFileInfo or <code>null</code>
	 */
	@Nullable
	public static IFileInfo getFileInfo(final IPath pathName) {
		final IFileStore store = getFileStore(pathName.toFile().toURI());
		if (store == null) {
			return null;
		}
		return store.fetchInfo();
	}
	
	/**
	 * Get the file store for the URI.	 
	 * Copied from {@code org.eclipse.ui.internal.ide.dialogs.IDEResourceInfoUtils}
	 *
	 * @param uri the Resource URI
	 * @return IFileStore or <code>null</code> if there is a
	 *         {@link CoreException}
	 */
	@Nullable
	public static IFileStore getFileStore(final URI uri) {
		try {
			return EFS.getStore(uri);
		} catch (final CoreException e) {
			Logging.error("Error while retrieving the file information: " + uri.getPath(), e);
			return null;
		}
	}
}
