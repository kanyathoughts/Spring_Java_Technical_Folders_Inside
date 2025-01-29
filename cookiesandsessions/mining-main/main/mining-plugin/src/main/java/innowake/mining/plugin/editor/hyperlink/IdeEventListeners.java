/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.hyperlink;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;

import innowake.lib.core.api.lang.Nullable;

/**
 * Class includes event listeners for eclipse events namely part listener and document listener.
 */
public class IdeEventListeners {
	
	private final IPartListener partListener;
	private final IDocumentListener documentListener;
	private final List<IdeEventCallBackHandler> callBacks = new ArrayList<>();
	
	/**
	 * Returns the instance of {@link IdeEventListeners}
	 *
	 * @return instance of {@link IdeEventListeners}
	 */
	public static IdeEventListeners getInstance() {
		return LazyHolder.INSTANCE;
	}
	
	/**
	 * Returns the document listener to be registered to the document.
	 *
	 * @return document listener to be registered
	 */
	public IDocumentListener getDocumentListener() {
		return documentListener;
	}
	
	/**
	 * Returns the part listener to be registered to the workbench.
	 *
	 * @return part listener to be registered
	 */
	public IPartListener getPartListener() {
		return partListener;
	}
	
	/**
	 * Registers the callback handler for the eclipse events.
	 *
	 * @param callBack callback handler for the events
	 */
	public void registerCallBack(final IdeEventCallBackHandler callBack) {
		this.callBacks.add(callBack);
	}
	
	private IdeEventListeners() {
		partListener = createPartListener();
		documentListener = createDocumentListener();
	}
	
	private static class LazyHolder {
		private static final IdeEventListeners INSTANCE = new IdeEventListeners(); 
	}
	
	private IPartListener createPartListener() {
		return new IPartListener() {

			@Override
			public void partOpened(@Nullable final IWorkbenchPart part) {
				callBacks.forEach(listener -> listener.partOpened(part));
			}

			@Override
			public void partDeactivated(@Nullable final IWorkbenchPart part) {
				/*not implemented*/
			}

			@Override
			public void partClosed(@Nullable final IWorkbenchPart part) {
				callBacks.forEach(listener -> listener.partClosed(part));
			}

			@Override
			public void partBroughtToTop(@Nullable final IWorkbenchPart part) {
				/*not implemented*/
			}

			@Override
			public void partActivated(@Nullable final IWorkbenchPart part) {
				/*not implemented*/
			}
		};
	}
	
	protected IDocumentListener createDocumentListener() {
		return new IDocumentListener() {
			@Override
			public void documentChanged(@Nullable final DocumentEvent event) {
				callBacks.forEach(listener -> listener.documentChanged(event));
			}
			@Override
			public void documentAboutToBeChanged(@Nullable final DocumentEvent event) {
				/*not implemented*/
			}
		};
	}
	
}
