/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.ReferenceKey;
import fw2.orm.xlsx.XlsxPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Reference Key</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.impl.ReferenceKeyImpl#getKeyColumnName <em>Key Column Name</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ReferenceKeyImpl extends MapElementImpl implements ReferenceKey {
	/**
	 * The default value of the '{@link #getKeyColumnName() <em>Key Column Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKeyColumnName()
	 * @generated
	 * @ordered
	 */
	protected static final String KEY_COLUMN_NAME_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getKeyColumnName() <em>Key Column Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKeyColumnName()
	 * @generated
	 * @ordered
	 */
	protected String keyColumnName = KEY_COLUMN_NAME_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ReferenceKeyImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return XlsxPackage.Literals.REFERENCE_KEY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getKeyColumnName() {
		return keyColumnName;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setKeyColumnName(String newKeyColumnName) {
		String oldKeyColumnName = keyColumnName;
		keyColumnName = newKeyColumnName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, XlsxPackage.REFERENCE_KEY__KEY_COLUMN_NAME, oldKeyColumnName, keyColumnName));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case XlsxPackage.REFERENCE_KEY__KEY_COLUMN_NAME:
				return getKeyColumnName();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case XlsxPackage.REFERENCE_KEY__KEY_COLUMN_NAME:
				setKeyColumnName((String)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case XlsxPackage.REFERENCE_KEY__KEY_COLUMN_NAME:
				setKeyColumnName(KEY_COLUMN_NAME_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case XlsxPackage.REFERENCE_KEY__KEY_COLUMN_NAME:
				return KEY_COLUMN_NAME_EDEFAULT == null ? keyColumnName != null : !KEY_COLUMN_NAME_EDEFAULT.equals(keyColumnName);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (keyColumnName: ");
		result.append(keyColumnName);
		result.append(')');
		return result.toString();
	}

} //ReferenceKeyImpl
