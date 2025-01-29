/**
 */
package fw2.model2.impl;

import fw2.model2.DbColumnMap;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Db Column Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DbColumnMapImpl#getDbColumnName <em>Db Column Name</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DbColumnMapImpl extends PrimitiveImpl implements DbColumnMap {
	/**
     * The default value of the '{@link #getDbColumnName() <em>Db Column Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDbColumnName()
     * @generated
     * @ordered
     */
	protected static final String DB_COLUMN_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getDbColumnName() <em>Db Column Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDbColumnName()
     * @generated
     * @ordered
     */
	protected String dbColumnName = DB_COLUMN_NAME_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DbColumnMapImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DB_COLUMN_MAP;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getDbColumnName() {
        return dbColumnName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDbColumnName(String newDbColumnName) {
        String oldDbColumnName = dbColumnName;
        dbColumnName = newDbColumnName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN_MAP__DB_COLUMN_NAME, oldDbColumnName, dbColumnName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.DB_COLUMN_MAP__DB_COLUMN_NAME:
                return getDbColumnName();
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
            case Model2Package.DB_COLUMN_MAP__DB_COLUMN_NAME:
                setDbColumnName((String)newValue);
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
            case Model2Package.DB_COLUMN_MAP__DB_COLUMN_NAME:
                setDbColumnName(DB_COLUMN_NAME_EDEFAULT);
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
            case Model2Package.DB_COLUMN_MAP__DB_COLUMN_NAME:
                return DB_COLUMN_NAME_EDEFAULT == null ? dbColumnName != null : !DB_COLUMN_NAME_EDEFAULT.equals(dbColumnName);
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
        result.append(" (dbColumnName: ");
        result.append(dbColumnName);
        result.append(')');
        return result.toString();
    }

} //DbColumnMapImpl
