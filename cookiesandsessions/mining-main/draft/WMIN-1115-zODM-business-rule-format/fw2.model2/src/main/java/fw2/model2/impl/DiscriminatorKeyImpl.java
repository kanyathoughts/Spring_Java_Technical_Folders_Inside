/**
 */
package fw2.model2.impl;

import fw2.model2.DiscriminatorKey;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Discriminator Key</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DiscriminatorKeyImpl#getDiscriminatorValue <em>Discriminator Value</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DiscriminatorKeyImpl extends ModelElementImpl implements DiscriminatorKey {
	/**
     * The default value of the '{@link #getDiscriminatorValue() <em>Discriminator Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDiscriminatorValue()
     * @generated
     * @ordered
     */
	protected static final String DISCRIMINATOR_VALUE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getDiscriminatorValue() <em>Discriminator Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDiscriminatorValue()
     * @generated
     * @ordered
     */
	protected String discriminatorValue = DISCRIMINATOR_VALUE_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DiscriminatorKeyImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DISCRIMINATOR_KEY;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getDiscriminatorValue() {
        return discriminatorValue;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDiscriminatorValue(String newDiscriminatorValue) {
        String oldDiscriminatorValue = discriminatorValue;
        discriminatorValue = newDiscriminatorValue;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE, oldDiscriminatorValue, discriminatorValue));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE:
                return getDiscriminatorValue();
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
            case Model2Package.DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE:
                setDiscriminatorValue((String)newValue);
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
            case Model2Package.DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE:
                setDiscriminatorValue(DISCRIMINATOR_VALUE_EDEFAULT);
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
            case Model2Package.DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE:
                return DISCRIMINATOR_VALUE_EDEFAULT == null ? discriminatorValue != null : !DISCRIMINATOR_VALUE_EDEFAULT.equals(discriminatorValue);
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
        result.append(" (discriminatorValue: ");
        result.append(discriminatorValue);
        result.append(')');
        return result.toString();
    }

} //DiscriminatorKeyImpl
