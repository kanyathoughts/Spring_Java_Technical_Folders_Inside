/**
 */
package fw2.model2.impl;

import fw2.model2.ComponentMapping;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Component Mapping</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.ComponentMappingImpl#getClassName <em>Class Name</em>}</li>
 *   <li>{@link fw2.model2.impl.ComponentMappingImpl#getComponentId <em>Component Id</em>}</li>
 *   <li>{@link fw2.model2.impl.ComponentMappingImpl#getId <em>Id</em>}</li>
 *   <li>{@link fw2.model2.impl.ComponentMappingImpl#getTypeCode <em>Type Code</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ComponentMappingImpl extends MinimalEObjectImpl.Container implements ComponentMapping {
	/**
     * The default value of the '{@link #getClassName() <em>Class Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getClassName()
     * @generated
     * @ordered
     */
	protected static final String CLASS_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getClassName() <em>Class Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getClassName()
     * @generated
     * @ordered
     */
	protected String className = CLASS_NAME_EDEFAULT;

	/**
     * The default value of the '{@link #getComponentId() <em>Component Id</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getComponentId()
     * @generated
     * @ordered
     */
	protected static final String COMPONENT_ID_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getComponentId() <em>Component Id</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getComponentId()
     * @generated
     * @ordered
     */
	protected String componentId = COMPONENT_ID_EDEFAULT;

	/**
     * The default value of the '{@link #getId() <em>Id</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getId()
     * @generated
     * @ordered
     */
	protected static final String ID_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getId() <em>Id</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getId()
     * @generated
     * @ordered
     */
	protected String id = ID_EDEFAULT;

	/**
     * The default value of the '{@link #getTypeCode() <em>Type Code</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTypeCode()
     * @generated
     * @ordered
     */
	protected static final String TYPE_CODE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getTypeCode() <em>Type Code</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTypeCode()
     * @generated
     * @ordered
     */
	protected String typeCode = TYPE_CODE_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ComponentMappingImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.COMPONENT_MAPPING;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getClassName() {
        return className;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setClassName(String newClassName) {
        String oldClassName = className;
        className = newClassName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.COMPONENT_MAPPING__CLASS_NAME, oldClassName, className));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getComponentId() {
        return componentId;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setComponentId(String newComponentId) {
        String oldComponentId = componentId;
        componentId = newComponentId;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.COMPONENT_MAPPING__COMPONENT_ID, oldComponentId, componentId));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getId() {
        return id;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setId(String newId) {
        String oldId = id;
        id = newId;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.COMPONENT_MAPPING__ID, oldId, id));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getTypeCode() {
        return typeCode;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setTypeCode(String newTypeCode) {
        String oldTypeCode = typeCode;
        typeCode = newTypeCode;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.COMPONENT_MAPPING__TYPE_CODE, oldTypeCode, typeCode));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.COMPONENT_MAPPING__CLASS_NAME:
                return getClassName();
            case Model2Package.COMPONENT_MAPPING__COMPONENT_ID:
                return getComponentId();
            case Model2Package.COMPONENT_MAPPING__ID:
                return getId();
            case Model2Package.COMPONENT_MAPPING__TYPE_CODE:
                return getTypeCode();
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
            case Model2Package.COMPONENT_MAPPING__CLASS_NAME:
                setClassName((String)newValue);
                return;
            case Model2Package.COMPONENT_MAPPING__COMPONENT_ID:
                setComponentId((String)newValue);
                return;
            case Model2Package.COMPONENT_MAPPING__ID:
                setId((String)newValue);
                return;
            case Model2Package.COMPONENT_MAPPING__TYPE_CODE:
                setTypeCode((String)newValue);
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
            case Model2Package.COMPONENT_MAPPING__CLASS_NAME:
                setClassName(CLASS_NAME_EDEFAULT);
                return;
            case Model2Package.COMPONENT_MAPPING__COMPONENT_ID:
                setComponentId(COMPONENT_ID_EDEFAULT);
                return;
            case Model2Package.COMPONENT_MAPPING__ID:
                setId(ID_EDEFAULT);
                return;
            case Model2Package.COMPONENT_MAPPING__TYPE_CODE:
                setTypeCode(TYPE_CODE_EDEFAULT);
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
            case Model2Package.COMPONENT_MAPPING__CLASS_NAME:
                return CLASS_NAME_EDEFAULT == null ? className != null : !CLASS_NAME_EDEFAULT.equals(className);
            case Model2Package.COMPONENT_MAPPING__COMPONENT_ID:
                return COMPONENT_ID_EDEFAULT == null ? componentId != null : !COMPONENT_ID_EDEFAULT.equals(componentId);
            case Model2Package.COMPONENT_MAPPING__ID:
                return ID_EDEFAULT == null ? id != null : !ID_EDEFAULT.equals(id);
            case Model2Package.COMPONENT_MAPPING__TYPE_CODE:
                return TYPE_CODE_EDEFAULT == null ? typeCode != null : !TYPE_CODE_EDEFAULT.equals(typeCode);
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
        result.append(" (className: ");
        result.append(className);
        result.append(", componentId: ");
        result.append(componentId);
        result.append(", id: ");
        result.append(id);
        result.append(", typeCode: ");
        result.append(typeCode);
        result.append(')');
        return result.toString();
    }

} //ComponentMappingImpl
