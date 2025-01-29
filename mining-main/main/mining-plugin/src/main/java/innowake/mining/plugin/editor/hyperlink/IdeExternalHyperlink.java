package innowake.mining.plugin.editor.hyperlink;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;

/**
 * A class representing external hyperlink.
 */
public class IdeExternalHyperlink extends IdeHyperlink {

	private final String targetPath;
	private final IWorkbenchPage page;

	/**
	 * Constructor to initialize {@link IdeExternalHyperlink}.
	 * 
	 * @param region source region of the hyperlink
	 * @param page workbench page where the hyperlinks would be displayed
	 * @param targetRegion of the hyperlink
	 * @param targetPath of the hyperlink
	 */
	public IdeExternalHyperlink(final IRegion region, final IWorkbenchPage page, final IRegion targetRegion, final String targetPath) {
		super(region, targetRegion);
		this.targetPath = targetPath;
		this.page = page;
	}

	@Override
	public void open() {
		if ( ! targetPath.isEmpty()) {
			final IPath path = new Path(targetPath);
			final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
			try {
				final IEditorPart editor = IDE.openEditor(page, file, false);
				final IRegion targetRegion = getTargetRegion();
				if (editor instanceof AbstractDecoratedTextEditor) {
					((AbstractDecoratedTextEditor) editor).selectAndReveal(targetRegion.getOffset(), targetRegion.getLength());
				}
				editor.setFocus();
			} catch (final PartInitException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}