package innowake.mining.plugin.preferences;

import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

/**
 * This class extends the ScopedPreferenceStore adding methods for secure storage of preferences.
 * <p>
 * The values are saved in the {@link ISecurePreferences} facilities of Eclipse.
 * The values are saved hierarchically based on the given qualifier and {@link IScopeContext scope}.
 * Supported scopes are {@link InstanceScope} and {@link ProjectScope}.
 * <p>
 * Examples:
 * <p>
 * Given a qualifier of {@code my.qualifier} the values for the {@link InstanceScope} are saved as
 * {@code my.qualifier/fully.qualified.path.to.workspace}
 * <p>
 * and values for the {@link ProjectScope} with the project {@code my-project} are saved as
 * {@code my.qualifier/fully.qualified.path.to.workspace/my-project}.
 */
public class SecureScopedPreferenceStore extends ScopedPreferenceStore {

	/**
	 * The workspace path for usage as a node in the preferences.
	 * <p>
	 * This has all path separators replaced with dots. Any leading dot gets removed.
	 */
	private static final String WORKSPACE_PATH = StringUtils.removeStart(StringUtils.replaceChars(
			ResourcesPlugin.getWorkspace().getRoot().getLocation().toPortableString(), IPath.SEPARATOR, '.'), ".");

	/**
	 * The scoped context to which preferences are stored.
	 */
	private final IScopeContext context;
	
	/**
	 * The associated preference node's qualifier or ID.
	 */
	private final String qualifier;

	/**
	 * Boolean value indicating whether or not this store has changes to be
	 * saved.
	 */
	private boolean dirty;

	/**
	 * The default constructor.
	 *
	 * @param context the scope to store to, e.g., {@link InstanceScope#INSTANCE}.
	 * @param qualifier used to look up the preference node, e.g., the
	 *            bundle's ID.
	 */
	public SecureScopedPreferenceStore(final IScopeContext context, final String qualifier) {
		super(context, qualifier);

		this.context = context;
		this.qualifier = qualifier;
	}

	/**
	 * Gets the associated default secure preference node from the
	 * {@link SecurePreferencesFactory}.
	 *
	 * @return The preference node for default secure values.
	 */
	private ISecurePreferences getDefaultSecurePreferences() {
		return getSecurePreferences(DefaultScope.INSTANCE);
	}

	/**
	 * Gets the associated secure preference node from the
	 * {@link SecurePreferencesFactory}.
	 *
	 * @return The preference node for secure values.
	 */
	private ISecurePreferences getSecureStorePreferences() {
		return getSecurePreferences(context);
	}
	
	private ISecurePreferences getSecurePreferences(final IScopeContext context) {
		final ISecurePreferences preferences = SecurePreferencesFactory.getDefault();
		final ISecurePreferences rootPreferences = preferences.node(qualifier);
		
		switch (context.getName()) {
			case DefaultScope.SCOPE:
				return rootPreferences.node(context.getName());
			case InstanceScope.SCOPE:
				return rootPreferences.node(WORKSPACE_PATH);
			case ProjectScope.SCOPE:
				final IPath projectLocation = context.getLocation();
				/* The location points to the .settings file in the project and we just want the name of the project 
				 * therefore we don't take the last segment but the second to last segment */
				final String projectName = projectLocation.segment(projectLocation.segmentCount() - 2);
				return rootPreferences.node(WORKSPACE_PATH).node(projectName);
			default:
				throw new IllegalArgumentException("Unsupported scope context " + context.getName());
		}
	}

	/**
	 * Checks if secure store contains a value associated with
	 * the given key.
	 *
	 * @param name of the key.
	 * @return true if the key is associated with a value.
	 */
	public boolean containsSecure(final String name) {
		return Arrays.asList(getSecureStorePreferences().keys()).contains(name);
	}
	
	/**
	 * Retrieves the secure boolean value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated boolean value or the default boolean value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public boolean getSecureBoolean(final String name) throws StorageException {
		return getSecureStorePreferences().getBoolean(name, getDefaultSecureBoolean(name));
	}

	/**
	 * Retrieves the secure double value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated double value or the default double value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public double getSecureDouble(final String name) throws StorageException {
		return getSecureStorePreferences().getDouble(name, getDefaultSecureDouble(name));
	}

	/**
	 * Retrieves the secure float value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated float value or the default float value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public float getSecureFloat(final String name) throws StorageException {
		return getSecureStorePreferences().getFloat(name, getDefaultSecureFloat(name));
	}

	/**
	 * Retrieves the secure integer value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated integer value or the default integer value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public int getSecureInt(final String name) throws StorageException {
		return getSecureStorePreferences().getInt(name, getDefaultSecureInt(name));
	}

	/**
	 * Retrieves the secure long value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated long value or the default long value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public long getSecureLong(final String name) throws StorageException {
		return getSecureStorePreferences().getLong(name, getDefaultSecureLong(name));
	}

	/**
	 * Retrieves the secure string value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated string value or the default string value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public String getSecureString(final String name) throws StorageException {
		return getSecureStorePreferences().get(name, getDefaultSecureString(name));
	}

	/**
	 * Retrieves the default secure boolean value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated default boolean value or the default boolean value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public boolean getDefaultSecureBoolean(final String name) throws StorageException {
		return getDefaultSecurePreferences().getBoolean(name, BOOLEAN_DEFAULT_DEFAULT);
	}

	/**
	 * Retrieves the default secure double value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated default double value or the default default value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public double getDefaultSecureDouble(final String name) throws StorageException {
		return getDefaultSecurePreferences().getDouble(name, DOUBLE_DEFAULT_DEFAULT);
	}

	/**
	 * Retrieves the default secure float value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated default float value or the default float value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public float getDefaultSecureFloat(final String name) throws StorageException {
		return getDefaultSecurePreferences().getFloat(name, FLOAT_DEFAULT_DEFAULT);
	}

	/**
	 * Retrieves the default secure integer value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated default integer value or the default integer value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public int getDefaultSecureInt(final String name) throws StorageException {
		return getDefaultSecurePreferences().getInt(name, INT_DEFAULT_DEFAULT);
	}

	/**
	 * Retrieves the default secure long value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated default long value or the default long value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public long getDefaultSecureLong(String name) throws StorageException {
		return getDefaultSecurePreferences().getLong(name, LONG_DEFAULT_DEFAULT);
	}

	/**
	 * Retrieves the default secure string value associated with the given key.
	 *
	 * @param name of the key
	 * @return the associated default string value or the default string value.
	 * @throws StorageException thrown if an error occurs accessing the secure storage.
	 */
	public String getDefaultSecureString(final String name) throws StorageException {
		return getDefaultSecurePreferences().get(name, STRING_DEFAULT_DEFAULT);
	}
	
	/**
	 * Removes the given key from the secure store and resets the default value
	 * associated with that key.
	 *
	 * @param name Name of the key to remove.
	 * @throws StorageException thrown if an error occurs while accessing the secure storage.
	 */
	public void removeSecureValue(final String name) throws StorageException {
		setToSecureDefault(name);
		setToSecureDefaultDefault(name);
	}
	
	private void setToSecureDefaultDefault(final String name) throws StorageException {
		final String oldValue = getDefaultSecureString(name);
		getDefaultSecurePreferences().remove(name);
		if (!Objects.equals(oldValue, STRING_DEFAULT_DEFAULT)) {
			dirty = true;
		}
	}
	
	/**
	 * Checks whether the given key has an associated default value.
	 *
	 * @param key the name of the key
	 * @return true if the given key is associated with a default value.
	 */
	public boolean isSecureDefault(final String key) {
		return ! containsSecure(key);
	}

	/**
	 * Sets the default value associated with the given key.
	 * 
	 * @param name of the key
	 * @param defaultObject value to the set the default value to
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setSecureDefault(final String name, final String defaultObject) throws StorageException {
		getDefaultSecurePreferences().put(name, defaultObject, true);
	}

	/**
	 * Sets the default value associated with the given key.
	 * 
	 * @param name of the key
	 * @param defaultObject value to the set the default value to
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setSecureDefault(final String name, final boolean defaultObject) throws StorageException {
		getDefaultSecurePreferences().putBoolean(name, defaultObject, true);
	}

	/**
	 * Sets the default value associated with the given key.
	 * 
	 * @param name of the key
	 * @param defaultObject value to the set the default value to
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setSecureDefault(final String name, final double defaultObject) throws StorageException {
		getDefaultSecurePreferences().putDouble(name, defaultObject, true);
	}

	/**
	 * Sets the default value associated with the given key.
	 * 
	 * @param name of the key
	 * @param defaultObject value to the set the default value to
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setSecureDefault(final String name, final float defaultObject) throws StorageException {
		getDefaultSecurePreferences().putFloat(name, defaultObject, true);
	}

	/**
	 * Sets the default value associated with the given key.
	 * 
	 * @param name of the key
	 * @param defaultObject value to the set the default value to
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setSecureDefault(final String name, final int defaultObject) throws StorageException {
		getDefaultSecurePreferences().putInt(name, defaultObject, true);
	}

	/**
	 * Sets the default value associated with the given key.
	 * 
	 * @param name of the key
	 * @param defaultObject value to the set the default value to
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setSecureDefault(final String name, final long defaultObject) throws StorageException {
		getDefaultSecurePreferences().putLong(name, defaultObject, true);
	}

	/**
	 * Sets the value of the given key to the associated default value.
	 * 
	 * @param name of the key
	 * @throws StorageException if an error occurs accessing the secure storage
	 */
	public void setToSecureDefault(final String name) throws StorageException {
		final String oldValue = getSecureString(name);
		final String defaultValue = getDefaultSecureString(name);
		getSecureStorePreferences().remove(name);
		if ( ! Objects.equals(oldValue, defaultValue)) {
			dirty = true;
		}
	}
	
	@Override
	public boolean needsSaving() {
		return dirty || super.needsSaving();
	}

	@Override
	public void save() throws IOException {
		boolean secureFlushed = false;
		getSecureStorePreferences().flush();
		secureFlushed = true;

		if (secureFlushed) {
			super.save();
			dirty = false;
		}

		return;
	}

	/**
	 * Sets the current value of the boolean-valued, securely stored
	 * preference with the given name.
	 *
	 * @param name of the preference.
	 * @param value of the preference that must be stored securely.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void setSecureValue(final String name, final boolean value) throws StorageException {
		getSecureStorePreferences().putBoolean(name, value, true);
		dirty = true;
	}

	/**
	 * Sets the current value of the double-valued, securely stored
	 * preference with the given name.
	 *
	 * @param name of the preference.
	 * @param value of the preference that must be stored securely.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void setSecureValue(final String name, final double value) throws StorageException {
		getSecureStorePreferences().putDouble(name, value, true);
		dirty = true;
	}

	/**
	 * Sets the current value of the float-valued, securely stored
	 * preference with the given name.
	 *
	 * @param name of the preference.
	 * @param value of the preference that must be stored securely.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void setSecureValue(final String name, final float value) throws StorageException {
		getSecureStorePreferences().putFloat(name, value, true);
		dirty = true;
	}

	/**
	 * Sets the current value of the integer-valued, securely stored
	 * preference with the given name.
	 *
	 * @param name of the preference.
	 * @param value of the preference that must be stored securely.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void setSecureValue(final String name, final int value) throws StorageException {
		getSecureStorePreferences().putInt(name, value, true);
		dirty = true;
	}

	/**
	 * Sets the current value of the string-valued, securely stored
	 * preference with the given name.
	 *
	 * @param name of the preference.
	 * @param value of the preference that must be stored securely.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void setSecureValue(final String name, final String value) throws StorageException {
		getSecureStorePreferences().put(name, value, true);
		dirty = true;
	}

	/**
	 * Sets the current value of the long-valued, securely stored
	 * preference with the given name.
	 *
	 * @param name of the preference.
	 * @param value of the preference that must be stored securely.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void setSecureValue(final String name, final long value) throws StorageException {
		getSecureStorePreferences().putLong(name, value, true);
		dirty = true;
	}

	/**
	 * Moves the value of the given key from the preference store to the
	 * secure preference store. Previously associated values in the secure
	 * store will be overwritten.
	 *
	 * @param key name of the key.
	 * @throws StorageException thrown if an error occurs retrieving a secure preference.
	 */
	public void moveStringToSecure(final String key) throws StorageException {
		if (contains(key)) {
			final String value = getString(key);
			setSecureValue(key, value);
			setToDefault(key);
		}
	}
 }
