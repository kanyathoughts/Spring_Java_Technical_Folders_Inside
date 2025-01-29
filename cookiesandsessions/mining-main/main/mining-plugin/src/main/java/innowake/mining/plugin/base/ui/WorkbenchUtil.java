/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;

/**
 * Common null-safe utility methods for convenient access of workbench components.
 */
public final class WorkbenchUtil {

	private WorkbenchUtil() {}
	
    /**
	 * Returns whether the workbench is not available yet or not available 
	 * at all because eclipse runs in headless mode.
     * 
     * @return {@code true} if the workbench is running
	 */
    public static boolean isWorkbenchRunning() {
    	return PlatformUI.isWorkbenchRunning();
    }
    
    /**
     * Returns the workbench.
     * 
     * @return workbench the workbench
     * @throws IllegalStateException  if the workbench is not available yet or not available 
     * at all because eclipse runs in headless mode 
     */
    public static IWorkbench getWorkbench() {
   		return PlatformUI.getWorkbench();
    }
	
    /**
	 * Returns the workbench or {@code null} if the workbench is not available yet or not available 
	 * at all because eclipse runs in headless mode.
     * 
     * @return workbench the workbench, or {@code null} 
	 */
	public static @Nullable IWorkbench getWorkbenchOrNull() {
		if (isWorkbenchRunning()) {
			return PlatformUI.getWorkbench();
		}
		return null;
	}
	
	/**
	 * Saves all dirty editors in the workbench. Opens a dialog to prompt the user. Returns
	 * {@code true} if successful or no workbench is available at all (then there are no dirty editors).
	 * Returns {@code false} if the user has canceled the command.
	 * 
	 * @return {@code true} if the command succeeded, and {@code false} if the operation was canceled by the user or an
	 *         error occurred while saving
	 */
	public static boolean saveAllEditors() {
		return saveAllEditors(true);
	}
	
	/**
	 * Saves all dirty editors in the workbench. Opens a dialog to prompt the user if {@code confirm} is {@code true}. Returns
	 * {@code true} if successful or no workbench is available at all (then there are no dirty editors).
	 * Returns {@code false} if the user has canceled the command.
	 * 
	 * @param confirm {@code true} to ask the user before saving unsaved changes (recommended), and {@code false} to
	 *            save unsaved changes without asking
	 * @return {@code true} if the command succeeded, and {@code false} if the operation was canceled by the user or an
	 *         error occurred while saving
	 */
	public static boolean saveAllEditors(final boolean confirm) {
		final IWorkbench workbench = getWorkbenchOrNull();
		if (workbench != null) {
			return workbench.saveAllEditors(confirm);
		}
		return true;
	}
	
    /**
     * Returns the active workbench shell or {@code null} if there
     * is no active workbench shell (e. g. in headless mode).
     * 
     * @return the active shell or {@code null}
     */
    public static @Nullable Shell getActiveShell() {
        final IWorkbenchWindow window = getActiveWindow();
        if (window != null) {
        	return window.getShell();
        }
        return null;
    }

    /**
     * Returns the active workbench window or {@code null} if there
     * is no active workbench window (e. g. in headless mode).
     * 
     * @return the active workbench window or {@code null}
     */
    public static @Nullable IWorkbenchWindow getActiveWindow() {
        IWorkbenchWindow window = null;
        if (isWorkbenchRunning()) { /* false if headless or not started yet */
            final IWorkbench wb = PlatformUI.getWorkbench();
            if (wb != null) {
                window = wb.getActiveWorkbenchWindow();
            }
        }
        return window;
    }
    
    /**
     * Returns the active workbench page or {@code null} if there
     * is no active workbench page available (e. g. in headless mode or window is empty).
     * 
     * @return the active workbench page or {@code null}
     */
    public static @Nullable IWorkbenchPage getActivePage() {
        IWorkbenchPage page = null;
        final IWorkbenchWindow window = getActiveWindow();
        if (window != null) {        	
            page = window.getActivePage();
        }
        return page;
    }
    
    /**
     * Returns the active workbench part or {@code null} if there
     * is no active editor part is available 
     * (e. g. no editor is open or eclipse was started  
     * in headless mode or window is empty).
     * 
     * @return the active editor or {@code null}
     */
    public static @Nullable IWorkbenchPart getActivePart() {
        final IWorkbenchPage page = getActivePage();
        if (page != null) {
            return page.getActivePart();
        }
        return null;
    }

    /**
     * Returns the active editor part or {@code null} if there
     * is no active editor part is available 
     * (e. g. no editor is open or eclipse was started  
     * in headless mode or window is empty).
     * 
     * @return the active editor or {@code null}
     */
    public static @Nullable IEditorPart getActiveEditor() {
        final IWorkbenchPage page = getActivePage();
        if (page != null) {
            return page.getActiveEditor();
        }
        return null;
    }
    
	/**
	 * Null-safe convenience method that returns the workbench page for the
	 * given workbench part or {@code null} if not available.
	 * 
	 * @param part the part or {@code null}
	 * @return the page or {@code null}
	 */
    public static @Nullable IWorkbenchPage getPageOrNull(@Nullable final IWorkbenchPart part) {
    	if (part != null) {
    		final IWorkbenchPartSite site = part.getSite();
    		if (site != null) {
    			return site.getPage();
    		}
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the workbench page for the
	 * given workbench part or the active page if no page is available
	 * for the given part.
	 * 
	 * @param part the part or {@code null}
	 * @return the page or {@code null} if even no active page is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IWorkbenchPage getPageOrDefault(final IWorkbenchPart part) {
    	final IWorkbenchPage result = getPageOrNull(part);
    	if (result == null) {
    		return getActivePage();
    	}
    	return result;
    }
    
	/**
	 * Null-safe convenience method that returns the workbench window for the
	 * given workbench part or {@code null} if not available.
	 * 
	 * @param part the part or {@code null}
	 * @return the page or {@code null}
	 */
    public static @Nullable IWorkbenchWindow getWindowOrNull(@Nullable final IWorkbenchPart part) {
    	if (part != null) {
    		final IWorkbenchPage page = getPageOrNull(part);
    		if (page != null) {
    			return page.getWorkbenchWindow();
    		}
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the workbench window for the
	 * given workbench part or the active page if no page is available
	 * for the given part.
	 * 
	 * @param part the part or {@code null}
	 * @return the page or {@code null} if even no active window is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IWorkbenchWindow getWindowOrDefault(@Nullable final IWorkbenchPart part) {
    	final IWorkbenchWindow result = getWindowOrNull(part);
    	if (result == null) {
    		return getActiveWindow();
    	}
    	return result;
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench part or {@code null} if it is not available.
	 * 
	 * @param part the part or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrNull(@Nullable final IWorkbenchPart part) {
    	if (part != null) {
    		final IWorkbenchSite site = part.getSite();
    		return getShellOrNull(site);
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench site or {@code null} if no shell is available
	 * for the given site.
	 * 
	 * @param site the site or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrNull(@Nullable final IWorkbenchSite site) {
    	if (site != null) {
    		return site.getShell();
    	}
    	return null;
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench site or {@code null} if no shell is available
	 * for the given page.
	 * 
	 * @param page the page or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrNull(@Nullable final IWorkbenchPage page) {
    	if (page != null) {
    		return getShellOrNull(page.getWorkbenchWindow());
    	}
    	return null;
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench window or {@code null} if no shell is available
	 * for the given window.
	 * 
	 * @param window the window or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrNull(@Nullable final IWorkbenchWindow window) {
    	if (window != null) {
    		return window.getShell();
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the shell for the
	 * given control or {@code null} if no shell is available
	 * for the given control.
	 * 
	 * @param control the control or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrNull(@Nullable final Control control) {
    	if (control != null) {
    		return control.getShell();
    	}
    	return null;
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given viewer or {@code null} if no shell is available
	 * for the given viewer.
	 * 
	 * @param viewer the viewer or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrNull(@Nullable final Viewer viewer) {
    	if (viewer != null) {
    		return getShellOrNull(viewer.getControl());
    	}
    	return null;
    }

	/**
	 * Null-safe convenience method that returns this shell or the
	 * active shell if the given shell is {@code null}. 
	 * 
	 * @param shell the shell or {@code null}
	 * @return the shell or {@code null} if even no active shell is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final Shell shell) {
    	if (shell == null) {
    		return getActiveShell();
    	}
    	return shell;
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench part or the active shell if no shell is available
	 * for the given part.
	 * 
	 * @param part the part or {@code null}
	 * @return the shell or {@code null} if even no active shell is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final IWorkbenchPart part) {
    	if (part != null) {
    		final IWorkbenchSite site = part.getSite();
    		return getShellOrDefault(site);
    	}
    	return getActiveShell();
    }
    
	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench page or the active shell if no shell is available
	 * for the given page.
	 * 
	 * @param page the page or {@code null}
	 * @return the shell or {@code null} if even no active shell is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final IWorkbenchPage page) {
    	if (page != null) {
    		return getShellOrDefault(page.getWorkbenchWindow());
    	}
    	return getActiveShell();
    }
    
	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench window or the active shell if no shell is available
	 * for the given window.
	 * 
	 * @param window the part or {@code null}
	 * @return the shell or {@code null} if even no active shell is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final IWorkbenchWindow window) {
    	if (window != null) {
    		return window.getShell();
    	}
    	return getActiveShell();
    }
    
	/**
	 * Null-safe convenience method that returns the shell for the
	 * given workbench site or {@code null} if it is not available.
	 * 
	 * @param site the site or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final IWorkbenchSite site) {
    	if (site != null) {
    		return site.getShell();
    	}
    	return getActiveShell();
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given control or {@code null} if it is not available.
	 * 
	 * @param control the control or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final Control control) {
    	if (control != null) {
    		return control.getShell();
    	}
    	return getActiveShell();
    }

	/**
	 * Null-safe convenience method that returns the shell for the
	 * given control or {@code null} if it is not available.
	 * viewer
	 * @param viewer the viewer or {@code null}
	 * @return the shell or {@code null}
	 */
    public static @Nullable Shell getShellOrDefault(@Nullable final Viewer viewer) {
    	if (viewer != null) {
    		return getShellOrNull(viewer.getControl());
    	}
    	return getActiveShell();
    }
    
	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench part or {@code null} if it is not available.
	 * 
	 * @param part the part or {@code null}
	 * @return the editor or {@code null}
	 */
    public static @Nullable IEditorPart getEditorOrNull(@Nullable final IWorkbenchPart part) {
    	if (part != null) {
    		final IWorkbenchSite site = part.getSite();
    		return getEditorOrNull(site);
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench site or the active editor if no editor is available
	 * for the given site.
	 * 
	 * @param site the site or {@code null}
	 * @return the editor or {@code null}
	 */
    public static @Nullable IEditorPart getEditorOrNull(@Nullable final IWorkbenchSite site) {
    	if (site != null) {    		
    		return getEditorOrNull(site.getPage());
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench page or {@code null} if no editor is available
	 * for the given page.
	 * 
	 * @param page the page or {@code null}
	 * @return the editor or {@code null} 
	 */
    public static @Nullable IEditorPart getEditorOrNull(@Nullable final IWorkbenchPage page) {
    	if (page != null) {
    		return page.getActiveEditor();
    	}
    	return null;
    }


	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench part or the active editor if no editor is available
	 * for the given part.
	 * 
	 * @param part the part or {@code null}
	 * @return the editor or {@code null} if even no active editor is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IEditorPart getEditorOrDefault(@Nullable final IWorkbenchPart part) {
    	if (part != null) {
    		final IWorkbenchSite site = part.getSite();
    		return getEditorOrDefault(site);
    	}
    	return getActiveEditor();
    }
    
	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench site or the active editor if no editor is available
	 * for the given site.
	 * 
	 * @param site the site or {@code null}
	 * @return the editor or {@code null} if even no active editor is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IEditorPart getEditorOrDefault(@Nullable final IWorkbenchSite site) {
    	if (site != null) {
    		return getEditorOrDefault(site.getPage());
    	}
    	return getActiveEditor();
    }

	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench page or the active editor if no editor is available
	 * for the given page.
	 * 
	 * @param page the page or {@code null}
	 * @return the editor or {@code null} if even no active editor is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IEditorPart getEditorOrDefault(@Nullable final IWorkbenchPage page) {
    	if (page != null) {
    		return page.getActiveEditor();
    	}
    	return getActiveEditor();
    }
    
	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench site or the active editor if no editor is available
	 * for the given site.
	 * 
	 * @param site the site or {@code null}
	 * @return the editor or {@code null}
	 */
    public static @Nullable IWorkbenchPart getPartOrNull(@Nullable final IWorkbenchSite site) {
    	if (site != null) {    		
    		return getPartOrNull(site.getPage());
    	}
    	return null;
    }
    
	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench page or {@code null} if no editor is available
	 * for the given page.
	 * 
	 * @param page the page or {@code null}
	 * @return the editor or {@code null} 
	 */
    public static @Nullable IWorkbenchPart getPartOrNull(@Nullable final IWorkbenchPage page) {
    	if (page != null) {
    		return page.getActivePart();
    	}
    	return null;
    }

	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench site or the active editor if no editor is available
	 * for the given site.
	 * 
	 * @param site the site or {@code null}
	 * @return the editor or {@code null} if even no active editor is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IWorkbenchPart getPartOrDefault(@Nullable final IWorkbenchSite site) {
    	if (site != null) {
    		return getPartOrDefault(site.getPage());
    	}
    	return getActivePart();
    }

	/**
	 * Null-safe convenience method that returns the editor for the
	 * given workbench page or the active editor if no editor is available
	 * for the given page.
	 * 
	 * @param page the page or {@code null}
	 * @return the editor or {@code null} if even no active editor is available
	 *         (e. g. in headless mode)
	 */
    public static @Nullable IWorkbenchPart getPartOrDefault(@Nullable final IWorkbenchPage page) {
    	if (page != null) {
    		return page.getActivePart();
    	}
    	return getActivePart();
    }
    
    /**
     * Null-safe method that returns the most obvious display to be used. The method first checks, if
     * the thread calling this method has an associated display. If so, this
     * display is returned. Otherwise the method returns the default display.
     * 
     * @return the most obvious display; not {@code null}
     */
    public static Display getDisplaySafely() {
        Display display = Display.getCurrent();
        if (display == null) {
        	/* Could not be null */
            display = Display.getDefault();
        }
        return display;
    }
    
    /**
     * Null-safe method that returns the most obvious display if the
     * given display is {@code null} other wise the display itself. 
     * The given display can be {@code null} or disposed.
     * 
     * @param display the available display or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(@Nullable final Display display) {
    	if (display != null && ! display.isDisposed()) {    		
    		/* Liefert nie null zurück */
    		return display;
    	}
        return getDisplaySafely();
    }
    
    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given shell. The shell can also be {@code null} or disposed.
     * 
     * @param shell the available shell or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(@Nullable final Shell shell) {
    	Display display = null;
    	if (shell != null && ! shell.isDisposed()) {    		
    		display = shell.getDisplay();
    	}
    	return getDisplayFor(display);
    }
    
    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given part. The part can also be {@code null}.
     * 
     * @param part the available part or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(final IWorkbenchPart part) {
    	return getDisplayFor(getShellOrDefault(part));
    }

    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given page. The page can also be {@code null}.
     * 
     * @param page the available page or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(final IWorkbenchPage page) {
    	return getDisplayFor(getShellOrDefault(page));
    }

    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given site. The site can also be {@code null}.
     * 
     * @param site the available site or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(final IWorkbenchSite site) {
    	return getDisplayFor(getShellOrDefault(site));
    }
    
    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given window. The window can also be {@code null}.
     * 
     * @param window the available window or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(final IWorkbenchWindow window) {
    	return getDisplayFor(getShellOrDefault(window));
    }

    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given control. The control can also be {@code null}.
     * 
     * @param control the available control or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(final Control control) {
    	return getDisplayFor(getShellOrDefault(control));
    }
    
    /**
     * Null-safe method that returns the most obvious display to be used for the
     * given viewer. The viewer can also be {@code null}.
     * 
     * @param viewer the available viewer or {@code null}
     * @return the most obvious display; not {@code null}
     * 
     * @see #getDisplaySafely()
     */
    public static Display getDisplayFor(final Viewer viewer) {
    	return getDisplayFor(getShellOrDefault(viewer));
    }
    
    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the given display if it is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param display the display or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(@Nullable final Display display, @Nullable final Runnable runnable) {
    	if (runnable == null) {
    		return false;
    	}
    	final Display display2 = getDisplayFor(display);    	
    	if (! display2.isDisposed()) {
    		if (Thread.currentThread() != display2.getThread()) {
				display2.syncExec(runnable);
    		} else {
    			/* Call directly if this thread is the display thread.
    			 * The display does this internally anyway */
				runnable.run();
    		}
    		return true;
    	}
    	return false;
    }
    
    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the shell's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param shell the shell or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final Shell shell, final Runnable runnable) {
    	return syncExec(getDisplayFor(shell), runnable);
    }

    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the part's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param part the part or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final IWorkbenchPart part, final Runnable runnable) {
    	return syncExec(getDisplayFor(part), runnable);
    }

    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the site's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param site the site or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final IWorkbenchSite site, final Runnable runnable) {
    	return syncExec(getDisplayFor(site), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the page's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param page the page or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final IWorkbenchPage page, final Runnable runnable) {
    	return syncExec(getDisplayFor(page), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the window's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param window the window or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final IWorkbenchWindow window, final Runnable runnable) {
    	return syncExec(getDisplayFor(window), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the control's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param control the control or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final Control control, final Runnable runnable) {
    	return syncExec(getDisplayFor(control), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#syncExec(Runnable)}
     * on the viewer's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param viewer the viewer or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#syncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#syncExec(Runnable)
     */
    public static boolean syncExec(final Viewer viewer, final Runnable runnable) {
    	return syncExec(getDisplayFor(viewer), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the given display if it is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param display the display or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(@Nullable final Display display, @Nullable final Runnable runnable) {
    	final Display display2 = getDisplayFor(display);
    	if ( ! display2.isDisposed()) {
    		/* Runnable is allowed to be null */
    		display2.asyncExec(runnable);
    		return true;
    	}
    	return false;
    }
    
    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the shell's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param shell the shell or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final Shell shell, final Runnable runnable) {
    	return asyncExec(getDisplayFor(shell), runnable);
    }

    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the part's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param part the part or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final IWorkbenchPart part, final Runnable runnable) {
    	return asyncExec(getDisplayFor(part), runnable);
    }

    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the site's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param site the site or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final IWorkbenchSite site, final Runnable runnable) {
    	return asyncExec(getDisplayFor(site), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the page's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param page the page or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final IWorkbenchPage page, final Runnable runnable) {
    	return asyncExec(getDisplayFor(page), runnable);
    }
    
    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the window's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param window the window or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final IWorkbenchWindow window, final Runnable runnable) {
    	return asyncExec(getDisplayFor(window), runnable);
    }

    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the control's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param control the control or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final Control control, final Runnable runnable) {
    	return asyncExec(getDisplayFor(control), runnable);
    }

    /**
     * Null-safe method that calls {@link Display#asyncExec(Runnable)}
     * on the viewer's display if the display is neither {@code null} nor 
     * {@linkplain Display#dispose() disposed}.
     *
     * @param viewer the viewer or {@code null}
     * @param runnable the runnable or {@code null}
     * @return {@code true} if {@link Display#asyncExec(Runnable)} was called
     * 
     * @throws SWTException {@link SWT#ERROR_FAILED_EXEC} if an exception occurred 
     *          when executing the runnable
     * @see Display#asyncExec(Runnable)
     */
    public static boolean asyncExec(final Viewer viewer, final Runnable runnable) {
    	return asyncExec(getDisplayFor(viewer), runnable);
    }

	/**
	 * Closes the view identified by the given view ID on the active workbench page.
	 *
	 * @param viewId the ID of the view which should be closed
	 */
	public static void closeView(final String viewId) {
		final IWorkbenchPage page = getActivePage();
		if (page == null) {
			return;
		}
		final IViewPart part = page.findView(viewId);
		if (part != null) {
			page.hideView(part);
		}
	}
	
	/**
	 * Brings the workbench part on the active page to top.
	 *
	 * @param part the part to bring to top
	 */
	public static void bringToTop(final IWorkbenchPart part) {
		final IWorkbenchPage activePage = getActivePage();
		if (activePage != null) {
			activePage.bringToTop(part);
		} else {
			Logging.warn("Could not bring workbenchpart '" + part.getTitle() + "' to top, as there is no active workbench page available.");
		}
	}
	
	/**
	 * Opens the view with provided view ID and brings it to top without changing the focus
	 *
	 * @param viewId the view ID
	 */
	public static void openView(final String viewId) {
		final IWorkbenchPage page = getActivePage();
		if (page == null) {
			return;
		}
		try {
			bringToTop(page.showView(viewId, null, IWorkbenchPage.VIEW_VISIBLE));
		} catch (final PartInitException e) {
			Logging.error(e.getLocalizedMessage(), e);
		}
	}
}

