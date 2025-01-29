/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.function.Consumer;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import innowake.lib.core.api.lang.Nullable;

/**
 * Editor action to link a view to the editor. To be used as toggle toolbar button.
 * The image {@link ISharedImages#IMG_ELCL_SYNCED} is used.
 */
public class LinkWithEditorAction extends Action implements IPropertyChangeListener {

	private final Consumer<Boolean> onChange;
	private boolean state;

	/**
	 * Create a new "Link with Editor" action.
	 *  
	 * @param state The initial state of the link.
	 * @param onChange Consumer to be notified if the state change.
	 */
	public LinkWithEditorAction(final boolean state, final Consumer<Boolean> onChange) {
		this.state = state;
		this.onChange = onChange;
		setChecked(state);
		setImageDescriptor(getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_SYNCED));
		setDisabledImageDescriptor(getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_SYNCED_DISABLED));
		setToolTipText("Enable or disable the link between the view and the editor.");
		addPropertyChangeListener(this);
	}
	
	@Override
	public void propertyChange(@Nullable final PropertyChangeEvent event) {
		if (event != null && IAction.CHECKED.equals(event.getProperty())) {
			state = ! state;
			onChange.accept(Boolean.valueOf(state));
			setChecked(state);
		}
	}
	
	private static ISharedImages getSharedImages() {
		return PlatformUI.getWorkbench().getSharedImages();
	}
}