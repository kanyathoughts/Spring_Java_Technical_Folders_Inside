/**
 */
package fw2.model2.impl;

import fw2.model2.DomainAttribute;
import fw2.model2.DomainClass;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Domain Attribute</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DomainAttributeImpl#getDataType <em>Data Type</em>}</li>
 *   <li>{@link fw2.model2.impl.DomainAttributeImpl#isMultivalued <em>Multivalued</em>}</li>
 *   <li>{@link fw2.model2.impl.DomainAttributeImpl#isReference <em>Reference</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DomainAttributeImpl extends ModelElementImpl implements DomainAttribute {
	/**
     * The cached value of the '{@link #getDataType() <em>Data Type</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDataType()
     * @generated
     * @ordered
     */
	protected DomainClass dataType;

	/**
     * The default value of the '{@link #isMultivalued() <em>Multivalued</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isMultivalued()
     * @generated
     * @ordered
     */
	protected static final boolean MULTIVALUED_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isMultivalued() <em>Multivalued</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isMultivalued()
     * @generated
     * @ordered
     */
	protected boolean multivalued = MULTIVALUED_EDEFAULT;

	/**
     * The default value of the '{@link #isReference() <em>Reference</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isReference()
     * @generated
     * @ordered
     */
	protected static final boolean REFERENCE_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isReference() <em>Reference</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isReference()
     * @generated
     * @ordered
     */
	protected boolean reference = REFERENCE_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DomainAttributeImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DOMAIN_ATTRIBUTE;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DomainClass getDataType() {
        return dataType;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDataType(DomainClass newDataType) {
        DomainClass oldDataType = dataType;
        dataType = newDataType;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_ATTRIBUTE__DATA_TYPE, oldDataType, dataType));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isMultivalued() {
        return multivalued;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setMultivalued(boolean newMultivalued) {
        boolean oldMultivalued = multivalued;
        multivalued = newMultivalued;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_ATTRIBUTE__MULTIVALUED, oldMultivalued, multivalued));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isReference() {
        return reference;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setReference(boolean newReference) {
        boolean oldReference = reference;
        reference = newReference;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_ATTRIBUTE__REFERENCE, oldReference, reference));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.DOMAIN_ATTRIBUTE__DATA_TYPE:
                return getDataType();
            case Model2Package.DOMAIN_ATTRIBUTE__MULTIVALUED:
                return isMultivalued();
            case Model2Package.DOMAIN_ATTRIBUTE__REFERENCE:
                return isReference();
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
            case Model2Package.DOMAIN_ATTRIBUTE__DATA_TYPE:
                setDataType((DomainClass)newValue);
                return;
            case Model2Package.DOMAIN_ATTRIBUTE__MULTIVALUED:
                setMultivalued((Boolean)newValue);
                return;
            case Model2Package.DOMAIN_ATTRIBUTE__REFERENCE:
                setReference((Boolean)newValue);
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
            case Model2Package.DOMAIN_ATTRIBUTE__DATA_TYPE:
                setDataType((DomainClass)null);
                return;
            case Model2Package.DOMAIN_ATTRIBUTE__MULTIVALUED:
                setMultivalued(MULTIVALUED_EDEFAULT);
                return;
            case Model2Package.DOMAIN_ATTRIBUTE__REFERENCE:
                setReference(REFERENCE_EDEFAULT);
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
            case Model2Package.DOMAIN_ATTRIBUTE__DATA_TYPE:
                return dataType != null;
            case Model2Package.DOMAIN_ATTRIBUTE__MULTIVALUED:
                return multivalued != MULTIVALUED_EDEFAULT;
            case Model2Package.DOMAIN_ATTRIBUTE__REFERENCE:
                return reference != REFERENCE_EDEFAULT;
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
        result.append(" (multivalued: ");
        result.append(multivalued);
        result.append(", reference: ");
        result.append(reference);
        result.append(')');
        return result.toString();
    }

} //DomainAttributeImpl
