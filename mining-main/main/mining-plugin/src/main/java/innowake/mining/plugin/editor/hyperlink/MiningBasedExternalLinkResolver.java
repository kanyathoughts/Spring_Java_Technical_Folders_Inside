/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.hyperlink;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Region;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.module.FindAllLinkedModules;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.model.LinkedModule;
import innowake.mining.shared.model.ModuleLocation;

/**
 * External link resolver that depends on mining.
 */
public class MiningBasedExternalLinkResolver implements ExternalLinkResolver, IdeEventCallBackHandler {
	
	@Nullable private IProject currProject;
	@Nullable private IFile currFile;
	private MiningServiceExecutor<List<LinkedModule>> serviceExecutor;
	private final Map<IDocument, TreeMap<Integer, List<LinkedModule>>> moduleCache = new HashMap<>();

	@Override
	@Nullable
	public IdeExternalHyperlink resolve(final IDocument document, final int offset) {
		final TreeMap<Integer, List<LinkedModule>> moduleMapping = moduleCache.get(document);
		if (moduleMapping != null) {
			final Integer startOffset = Integer.valueOf(offset);
			final Integer floorKey = moduleMapping.floorKey(startOffset);
			try {
				if (floorKey != null) {
					final int offsetLine = document.getLineOfOffset(startOffset.intValue());
					final int hyperlinkLine = document.getLineOfOffset(floorKey.intValue()) ;
					if (offsetLine == hyperlinkLine) {
						final List<LinkedModule> modules = moduleMapping.get(floorKey);
						if (modules != null) {
							final LinkedModule module = modules.get(0);
							final ModuleLocation fromLocation = module.getFromLocation();
							final ModuleLocation toLocation = module.getToLocation();
							if (fromLocation != null && toLocation != null) {
								final Region fromRegion = new Region(fromLocation.getOffset().intValue(), fromLocation.getLength().intValue());
								final Region toRegion = new Region(toLocation.getOffset().intValue(), 0);
								final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
								final String currentProjectPath = getCurrentProjectPath();
								return new IdeExternalHyperlink(fromRegion, page, toRegion, currentProjectPath + "/" + module.getModulePath());
							}
						}
					}
				}
			} catch (final BadLocationException e) {
				throw new IllegalStateException("Not able to find the line information of the offset" + e, e);
			}
		}
		return null;
	}

	/**
	 * Returns the instance of {@link MiningBasedExternalLinkResolver}.
	 *
	 * @return instance of {@link MiningBasedExternalLinkResolver}
	 */
	public static MiningBasedExternalLinkResolver getInstance() {
		return LazyHolder.INSTANCE;
	}
	
	@Override
	public void partOpened(@Nullable final IWorkbenchPart part) {
		currProject = getProjectFromWorkbenchPart(part);
		currFile = getFilePathFromWorkbenchPart(part);
		if (currFile != null && currProject != null && MiningPreferences.getApiProject(currProject).isPresent()) {
			final Optional<List<LinkedModule>> modules = serviceExecutor.executeWithDefaultErrorHandling();
			final IDocument document = getDocumentFromWorkbenchPart(part);
			if (document != null && modules.isPresent()) {
				populateCache(document, modules.get());
			}
		}
	
	}

	@Override
	public void partClosed(@Nullable final IWorkbenchPart part) {
		final IDocument document = getDocumentFromWorkbenchPart(part);
		if (moduleCache.containsKey(document)) {
			moduleCache.get(document).clear();
		}
	}

	@Override
	public void documentChanged(@Nullable final DocumentEvent event) {
		/*Not implemented*/
	}
	
	private MiningBasedExternalLinkResolver() {
		IdeEventListeners.getInstance().registerCallBack(this);
		serviceExecutor = MiningServiceExecutor.create(this::findLinkedModule);
	}
	
	private FindAllLinkedModules findLinkedModule() {
		try {
			final String projectRelativePath = ResourceUtil.getProjectRelativePath(assertNotNull(currFile));
			return ApiClient.moduleService(assertNotNull(currProject))
			                .findAllLinkedModules()
			                .setPath(projectRelativePath);
		} catch (final CoreException | StorageException e) {
			throw new IllegalStateException(e);
		}
	}
	
	@Nullable
	private IDocument getDocumentFromWorkbenchPart(@Nullable final IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			final ITextEditor editor = part.getAdapter(ITextEditor.class);
			if (editor != null) {
				final IDocumentProvider provider = editor.getDocumentProvider();
				return provider.getDocument(editor.getEditorInput());
			}
		}
		return null;
	}
	
	@Nullable
	private IProject getProjectFromWorkbenchPart(@Nullable final IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			final ITextEditor editor = part.getAdapter(ITextEditor.class);
			if (editor != null) {
				final IEditorInput input = editor.getEditorInput();
				IProject project = input.getAdapter(IProject.class);
				if (project == null) {
					IResource resource = input.getAdapter(IResource.class);
					if (resource != null) {
						return resource.getProject();

					}
				}
			}
		}
		return null;
	}
	
	@Nullable
	private IFile getFilePathFromWorkbenchPart(@Nullable final IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			final ITextEditor editor = part.getAdapter(ITextEditor.class);
			if (editor != null) {
				final IEditorInput input = editor.getEditorInput();
				if (input instanceof IFileEditorInput) {
					return ((IFileEditorInput) input).getFile();
				}
			}
		}
		return null;
	}
	
	private static class LazyHolder {
		private static final MiningBasedExternalLinkResolver INSTANCE = new MiningBasedExternalLinkResolver(); 
	}
	
	private void populateCache(final IDocument document, final List<LinkedModule> linkedModules) {
		final TreeMap<Integer, List<LinkedModule>> moduleMapping = new TreeMap<>();
		for (final LinkedModule module : linkedModules) {
			final ModuleLocation fromLocation = module.getFromLocation();
			if (fromLocation!= null) {
			final Integer fromOffset = fromLocation.getOffset();
			moduleMapping.computeIfAbsent(fromOffset, key -> new ArrayList<>()).add(module);
			}
		}
		moduleCache.put(document, moduleMapping);
	}
	
	@Nullable
	private String getCurrentProjectPath() {
		final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		final IEditorPart activeEditor = activePage.getActiveEditor();
		if (activeEditor != null) {
			final IEditorInput input = activeEditor.getEditorInput();
			final IProject project = input.getAdapter(IProject.class);
			if (project == null) {
				final IResource resource = input.getAdapter(IResource.class);
				if (resource != null) {
					return resource.getProject().getFullPath().toString();
				}
			}
		}
		return null;
	}
	
}
