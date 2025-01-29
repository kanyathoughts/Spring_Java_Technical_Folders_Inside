/**
 */
package fw2.model2.impl;

import fw2.model2.Model2Package;
import fw2.model2.UiFieldType;
import fw2.model2.UiFieldTypeProperty;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Ui Field Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.UiFieldTypeImpl#getDataType <em>Data Type</em>}</li>
 *   <li>{@link fw2.model2.impl.UiFieldTypeImpl#getHtmlType <em>Html Type</em>}</li>
 *   <li>{@link fw2.model2.impl.UiFieldTypeImpl#getOnScreenValidationMethod <em>On Screen Validation Method</em>}</li>
 *   <li>{@link fw2.model2.impl.UiFieldTypeImpl#getProperties <em>Properties</em>}</li>
 * </ul>
 *
 * @generated
 */
public class UiFieldTypeImpl extends MinimalEObjectImpl.Container implements UiFieldType {
	/**
     * The default value of the '{@link #getDataType() <em>Data Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDataType()
     * @generated
     * @ordered
     */
	protected static final String DATA_TYPE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getDataType() <em>Data Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDataType()
     * @generated
     * @ordered
     */
	protected String dataType = DATA_TYPE_EDEFAULT;

	/**
     * The default value of the '{@link #getHtmlType() <em>Html Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHtmlType()
     * @generated
     * @ordered
     */
	protected static final String HTML_TYPE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getHtmlType() <em>Html Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHtmlType()
     * @generated
     * @ordered
     */
	protected String htmlType = HTML_TYPE_EDEFAULT;

	/**
     * The default value of the '{@link #getOnScreenValidationMethod() <em>On Screen Validation Method</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getOnScreenValidationMethod()
     * @generated
     * @ordered
     */
	protected static final String ON_SCREEN_VALIDATION_METHOD_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getOnScreenValidationMethod() <em>On Screen Validation Method</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getOnScreenValidationMethod()
     * @generated
     * @ordered
     */
	protected String onScreenValidationMethod = ON_SCREEN_VALIDATION_METHOD_EDEFAULT;

	/**
     * The cached value of the '{@link #getProperties() <em>Properties</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getProperties()
     * @generated
     * @ordered
     */
	protected EList<UiFieldTypeProperty> properties;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected UiFieldTypeImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.UI_FIELD_TYPE;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getDataType() {
        return dataType;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDataType(String newDataType) {
        String oldDataType = dataType;
        dataType = newDataType;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.UI_FIELD_TYPE__DATA_TYPE, oldDataType, dataType));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getHtmlType() {
        return htmlType;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setHtmlType(String newHtmlType) {
        String oldHtmlType = htmlType;
        htmlType = newHtmlType;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.UI_FIELD_TYPE__HTML_TYPE, oldHtmlType, htmlType));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getOnScreenValidationMethod() {
        return onScreenValidationMethod;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setOnScreenValidationMethod(String newOnScreenValidationMethod) {
        String oldOnScreenValidationMethod = onScreenValidationMethod;
        onScreenValidationMethod = newOnScreenValidationMethod;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD, oldOnScreenValidationMethod, onScreenValidationMethod));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<UiFieldTypeProperty> getProperties() {
        if (properties == null) {
            properties = new EObjectContainmentEList<UiFieldTypeProperty>(UiFieldTypeProperty.class, this, Model2Package.UI_FIELD_TYPE__PROPERTIES);
        }
        return properties;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.UI_FIELD_TYPE__PROPERTIES:
                return ((InternalEList<?>)getProperties()).basicRemove(otherEnd, msgs);
        }
        return super.eInverseRemove(otherEnd, featureID, msgs);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.UI_FIELD_TYPE__DATA_TYPE:
                return getDataType();
            case Model2Package.UI_FIELD_TYPE__HTML_TYPE:
                return getHtmlType();
            case Model2Package.UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD:
                return getOnScreenValidationMethod();
            case Model2Package.UI_FIELD_TYPE__PROPERTIES:
                return getProperties();
        }
        return super.eGet(featureID, resolve, coreType);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
        switch (featureID) {
            case Model2Package.UI_FIELD_TYPE__DATA_TYPE:
                setDataType((String)newValue);
                return;
            case Model2Package.UI_FIELD_TYPE__HTML_TYPE:
                setHtmlType((String)newValue);
                return;
            case Model2Package.UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD:
                setOnScreenValidationMethod((String)newValue);
                return;
            case Model2Package.UI_FIELD_TYPE__PROPERTIES:
                getProperties().clear();
                getProperties().addAll((Collection<? extends UiFieldTypeProperty>)newValue);
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
            case Model2Package.UI_FIELD_TYPE__DATA_TYPE:
                setDataType(DATA_TYPE_EDEFAULT);
                return;
            case Model2Package.UI_FIELD_TYPE__HTML_TYPE:
                setHtmlType(HTML_TYPE_EDEFAULT);
                return;
            case Model2Package.UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD:
                setOnScreenValidationMethod(ON_SCREEN_VALIDATION_METHOD_EDEFAULT);
                return;
            case Model2Package.UI_FIELD_TYPE__PROPERTIES:
                getProperties().clear();
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
            case Model2Package.UI_FIELD_TYPE__DATA_TYPE:
                return DATA_TYPE_EDEFAULT == null ? dataType != null : !DATA_TYPE_EDEFAULT.equals(dataType);
            case Model2Package.UI_FIELD_TYPE__HTML_TYPE:
                return HTML_TYPE_EDEFAULT == null ? htmlType != null : !HTML_TYPE_EDEFAULT.equals(htmlType);
            case Model2Package.UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD:
                return ON_SCREEN_VALIDATION_METHOD_EDEFAULT == null ? onScreenValidationMethod != null : !ON_SCREEN_VALIDATION_METHOD_EDEFAULT.equals(onScreenValidationMethod);
            case Model2Package.UI_FIELD_TYPE__PROPERTIES:
                return properties != null && !properties.isEmpty();
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
        result.append(" (dataType: ");
        result.append(dataType);
        result.append(", htmlType: ");
        result.append(htmlType);
        result.append(", onScreenValidationMethod: ");
        result.append(onScreenValidationMethod);
        result.append(')');
        return result.toString();
    }

} //UiFieldTypeImpl
