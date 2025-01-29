/**
 */
package fw2.model2.impl;

import fw2.model2.Model2Package;
import fw2.model2.UiFieldTypeProperty;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Ui Field Type Property</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.UiFieldTypePropertyImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link fw2.model2.impl.UiFieldTypePropertyImpl#getPropertyType <em>Property Type</em>}</li>
 *   <li>{@link fw2.model2.impl.UiFieldTypePropertyImpl#getTypeName <em>Type Name</em>}</li>
 * </ul>
 *
 * @generated
 */
public class UiFieldTypePropertyImpl extends MinimalEObjectImpl.Container implements UiFieldTypeProperty {
	/**
     * The default value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyName()
     * @generated
     * @ordered
     */
	protected static final String PROPERTY_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyName()
     * @generated
     * @ordered
     */
	protected String propertyName = PROPERTY_NAME_EDEFAULT;

	/**
     * The default value of the '{@link #getPropertyType() <em>Property Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyType()
     * @generated
     * @ordered
     */
	protected static final String PROPERTY_TYPE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getPropertyType() <em>Property Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyType()
     * @generated
     * @ordered
     */
	protected String propertyType = PROPERTY_TYPE_EDEFAULT;

	/**
     * The default value of the '{@link #getTypeName() <em>Type Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTypeName()
     * @generated
     * @ordered
     */
	protected static final String TYPE_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getTypeName() <em>Type Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTypeName()
     * @generated
     * @ordered
     */
	protected String typeName = TYPE_NAME_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected UiFieldTypePropertyImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.UI_FIELD_TYPE_PROPERTY;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getPropertyName() {
        return propertyName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPropertyName(String newPropertyName) {
        String oldPropertyName = propertyName;
        propertyName = newPropertyName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME, oldPropertyName, propertyName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getPropertyType() {
        return propertyType;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPropertyType(String newPropertyType) {
        String oldPropertyType = propertyType;
        propertyType = newPropertyType;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE, oldPropertyType, propertyType));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getTypeName() {
        return typeName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setTypeName(String newTypeName) {
        String oldTypeName = typeName;
        typeName = newTypeName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.UI_FIELD_TYPE_PROPERTY__TYPE_NAME, oldTypeName, typeName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME:
                return getPropertyName();
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE:
                return getPropertyType();
            case Model2Package.UI_FIELD_TYPE_PROPERTY__TYPE_NAME:
                return getTypeName();
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
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME:
                setPropertyName((String)newValue);
                return;
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE:
                setPropertyType((String)newValue);
                return;
            case Model2Package.UI_FIELD_TYPE_PROPERTY__TYPE_NAME:
                setTypeName((String)newValue);
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
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME:
                setPropertyName(PROPERTY_NAME_EDEFAULT);
                return;
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE:
                setPropertyType(PROPERTY_TYPE_EDEFAULT);
                return;
            case Model2Package.UI_FIELD_TYPE_PROPERTY__TYPE_NAME:
                setTypeName(TYPE_NAME_EDEFAULT);
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
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME:
                return PROPERTY_NAME_EDEFAULT == null ? propertyName != null : !PROPERTY_NAME_EDEFAULT.equals(propertyName);
            case Model2Package.UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE:
                return PROPERTY_TYPE_EDEFAULT == null ? propertyType != null : !PROPERTY_TYPE_EDEFAULT.equals(propertyType);
            case Model2Package.UI_FIELD_TYPE_PROPERTY__TYPE_NAME:
                return TYPE_NAME_EDEFAULT == null ? typeName != null : !TYPE_NAME_EDEFAULT.equals(typeName);
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
        result.append(" (propertyName: ");
        result.append(propertyName);
        result.append(", propertyType: ");
        result.append(propertyType);
        result.append(", typeName: ");
        result.append(typeName);
        result.append(')');
        return result.toString();
    }

} //UiFieldTypePropertyImpl
