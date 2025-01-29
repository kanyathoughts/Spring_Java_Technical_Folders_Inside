/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.io.IOException;

import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;

import innowake.lib.core.api.lang.Nullable;

/**
 * This class extends the StringFieldEditor adding the functionality to store and
 * manage secure preferences.
 */
public class SecureStringFieldEditor extends StringFieldEditor {

	/**
	 * Default constructor. 
	 * 
     * @param name the name of the preference this field editor works on
     * @param labelText the label text of the field editor
     * @param parent the parent of the field editor's control
	 */
	public SecureStringFieldEditor(final String name, final String labelText, final Composite parent) {
		super(name, labelText, parent);
	}
	
	@Override
    public void setPreferenceStore(final @Nullable IPreferenceStore store) {
		if (store != null && ! (store instanceof SecureScopedPreferenceStore)) {
			throw new IllegalArgumentException(
					"Got '" + store.getClass().getCanonicalName() + "' " +
					"but expected '" + SecureScopedPreferenceStore.class.getCanonicalName() + "'.");
		}
		super.setPreferenceStore(store);
    }
	
	private SecureScopedPreferenceStore getSecurePreferenceStore() {
		return (SecureScopedPreferenceStore) super.getPreferenceStore();
	}

    @Override
	protected void doLoad() {
        if (getTextControl() != null) {
        	final SecureScopedPreferenceStore store = getSecurePreferenceStore();
        	try {
				store.moveStringToSecure(getPreferenceName());
				getSecurePreferenceStore().save();
			} catch (StorageException | IOException e) {
				throw new IllegalStateException("Error while writing secure property: " + e.getMessage(), e);
			}
            String value;
			try {
				value = store.getSecureString(getPreferenceName());
			} catch (final StorageException e) {
				throw new IllegalStateException("Error while reading secure property: " + e.getMessage(), e);
			}
            getTextControl().setText(value);
            oldValue = value;
        }
    }

	@Override
	protected void doStore() {
		try {
			getSecurePreferenceStore().setSecureValue(getPreferenceName(), getTextControl().getText());
			getSecurePreferenceStore().save();
		} catch (final StorageException | IOException e) {
			throw new IllegalStateException("Error while saving secure property: " + e.getMessage(), e);
		}
    }
	
	@Override
    public String getStringValue() {
        if (getTextControl() != null) {
			return getTextControl().getText();
		}

        try {
			return getSecurePreferenceStore().getSecureString(getPreferenceName());
		} catch (final StorageException e) {
			throw new IllegalStateException("Error while getting secure property: " + e.getMessage(), e);
		}
    }
}
