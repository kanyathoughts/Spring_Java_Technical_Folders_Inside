/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchWindow;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.datadictionary.handler.TokenTextSelection;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.fieldtracing.model.TracedModule;
import innowake.ndt.ide.core.object.IIdeLegacyObject;

/**
 * Utility methods for {@link ISelection}.
 */
public final class SelectionUtil {

	private SelectionUtil() {}

	/**
	 * Get the current selected resource.
	 * May be {@link Optional#empty()} if the workbench assess failed or the selection
	 * was {@code null}.
	 * If the selection is of type {@link TextSelection} it will be wrapped and the current
	 * active editor file will be provided.
	 *
	 * @return The {@link ISelection} wrapped in an {@link Optional} or {@link Optional#empty()}.
	 */
	public static Optional<IStructuredSelection> getResourceSelection() {
		final IWorkbenchWindow workbench = WorkbenchUtil.getActiveWindow();
		if (workbench != null) {
			return Optional.ofNullable(wrapSelection(workbench.getSelectionService().getSelection()));
		}
		return Optional.empty();
	}

	/**
	 * If the current selection is of type {@link TextSelection}, it will be wrapped into a
	 * {@link IStructuredSelection} otherwise {@link Optional#empty()} will be provided.
	 *
	 * @return The wrapped {@link TextSelection} or {@link Optional#empty()}.
	 */
	public static Optional<IStructuredSelection> getTextSelection() {
		final IWorkbenchWindow workbench = WorkbenchUtil.getActiveWindow();
		if (workbench != null) {
			final ISelection selection = workbench.getSelectionService().getSelection();
			if (selection instanceof TextSelection) {
				return Optional.of(new TextSelectionWrapper((TextSelection) selection));
			}
		}
		return Optional.empty();
	}

	/**
	 * Extracts the {@link IProject} from a selection if possible. Returns {@link Optional#empty()} if the project could not be extracted.
	 *
	 * @param selection the selection
	 * @return the {@link IProject} or {@link Optional#empty()}
	 */
	public static Optional<IProject> getProject(final ISelection selection) {
		if (selection.isEmpty()) {
			return Optional.empty();
		} else if (selection instanceof IStructuredSelection) {
			final Object element = ((IStructuredSelection) selection).getFirstElement();
			if (element instanceof IProject) {
				return Optional.of((IProject) element);
			} else if (element instanceof IResource) {
				return Optional.of(((IResource) element).getProject());
			} else if (element instanceof IJavaProject) {
				return Optional.of(((IJavaProject) element).getProject());
			} else if (element instanceof IJavaElement) {
				return Optional.of(((IJavaElement) element).getResource().getProject());
			}
		}
		return Optional.empty();
	}

	/**
	 * Extracts the {@link IProject} from current resource selection if possible. Returns {@link Optional#empty()} if the project could not be extracted.
	 *
	 * @return the {@link IProject} or {@link Optional#empty()}
	 */
	public static Optional<IProject> getProjectFromSelectedResource() {
		final Optional<IStructuredSelection> selection = getResourceSelection();
		if (selection.isPresent()) {
			return getProject(selection.get());
		}
		return Optional.empty();
	}

	/**
	 * Extracts the {@link IFile} from a selection if possible. Returns {@link Optional#empty()} if the file could not be extracted.
	 *
	 * @param selection the selection
	 * @return the {@link IFile} or {@link Optional#empty()}
	 */
	public static Optional<IFile> getFile(final ISelection selection) {
		if (selection.isEmpty()) {
			return Optional.empty();
		} else if (selection instanceof IStructuredSelection) {
			final Object element = ((IStructuredSelection) selection).getFirstElement();
			if (element instanceof IFile) {
				return Optional.of((IFile) element);
			}
		}
		return Optional.empty();
	}

	/**
	 * Extracts the {@link IFile}s from a selection if possible. Returns an empty {@link List} if no files could be extracted.
	 *
	 * @param selection the selection
	 * @return the {@link List} of {@link IFile}s or an empty {@link List}
	 */
	public static List<IFile> getFiles(final ISelection selection) {
		if (selection.isEmpty() || ! (selection instanceof IStructuredSelection)) {
			return Collections.emptyList();
		}
		if (selection instanceof FileSelectionWrapper) {
			final Object[] element = new Object[] {((FileSelectionWrapper) selection).getFirstElement()};
			return resolveFiles(element);
		}
		return resolveFiles(((IStructuredSelection) selection).toList().toArray());
	}

	/**
	 * Extracts the file and folder paths for a selection.
	 * <p>
	 * The selection will not be resolved recursively but folders will get an ANT pattern suffix for recursing those folders.  
	 *
	 * @param selection the selection for retrieving the paths
	 * @return the list of file and folder paths, or an empty list
	 */
	public static List<String> getFileAndFolderPaths(final ISelection selection) {
		if (selection.isEmpty() || ! (selection instanceof IStructuredSelection)) {
			return Collections.emptyList();
		}
		if (selection instanceof FileSelectionWrapper) {
			final Object[] element = new Object[] {((FileSelectionWrapper) selection).getFirstElement()};
			return resolvePaths(element);
		}
		return resolvePaths(((IStructuredSelection) selection).toList().toArray());
	}

	/**
	 * Returns the selection based on the current token of the given selection.
	 * <p>
	 * This only returns a modified selection if the length of the given selection is zero. Otherwise the selection will be returned unmodified.
	 *
	 * @param ideObject the {@link IIdeLegacyObject} from which the token should be determined
	 * @param selection the user selection
	 * @return a text selection based on the current text token, or {@link Optional#empty()} if the token cannot be determined
	 */
	public static Optional<ITextSelection> getTokenSelection(final TracedModule<?> ideObject, final ITextSelection selection) {
		/* We assume the selection already contains the complete token when its length is not zero,
		 * so just return the selection unmodified */
		if (selection.getLength() != 0) {
			return Optional.of(selection);
		}

		final int initialOffset = selection.getOffset();
		final ITokenPartitioning tokenPartitioning;
		tokenPartitioning = ideObject.getTokenPartitioning();

		final IToken nearestToken = tokenPartitioning.getNearestToken(initialOffset);

		if (nearestToken == null) {
			return Optional.empty();
		}

		return Optional.of(new TokenTextSelection(nearestToken));
	}


	@Nullable
	private static IStructuredSelection wrapSelection(final ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			return (IStructuredSelection) selection;
		} else if (selection instanceof TextSelection) {
			return new FileSelectionWrapper(EditorActionUtil.getFile());
		}
		return null;
	}

	private static List<IFile> resolveFiles(final Object[] elements) {
		final List<IFile> files = new ArrayList<>();

		for (final Object element : elements) {
			if (element instanceof IContainer) {
				try {
					files.addAll(resolveFiles(((IContainer) element).members()));
				} catch (final CoreException e) {
					throw new IllegalStateException(e);
				}
			} else if (element instanceof IFile) {
				files.add((IFile) element);
			}
		}

		return files;
	}
	
	private static List<String> resolvePaths(final Object[] elements) {
		final List<String> paths = new ArrayList<>();

		for (final Object element : elements) {
			if (element instanceof IContainer) {
				final String path = ((IContainer) element).getProjectRelativePath().toPortableString();
				final String pathWithAntPattern = path + "/**/*";
				paths.add(pathWithAntPattern);
			} else if (element instanceof IFile) {
				final String path = ((IFile) element).getProjectRelativePath().toPortableString();
				paths.add(path);
			}
		}

		return paths;
	}

}

