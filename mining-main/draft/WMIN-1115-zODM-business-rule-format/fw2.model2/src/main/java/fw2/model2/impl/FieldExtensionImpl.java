/**
 */
package fw2.model2.impl;

import fw2.model2.FieldExtension;
import fw2.model2.Model2Package;
import fw2.model2.TypeExtension;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Field Extension</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.FieldExtensionImpl#getExtension <em>Extension</em>}</li>
 * </ul>
 *
 * @generated
 */
public class FieldExtensionImpl extends FieldImpl implements FieldExtension {
	/**
     * The cached value of the '{@link #getExtension() <em>Extension</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getExtension()
     * @generated
     * @ordered
     */
	protected TypeExtension extension;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected FieldExtensionImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.FIELD_EXTENSION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public TypeExtension getExtension() {
        return extension;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public NotificationChain basicSetExtension(TypeExtension newExtension, NotificationChain msgs) {
        TypeExtension oldExtension = extension;
        extension = newExtension;
        if (eNotificationRequired()) {
            ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, Model2Package.FIELD_EXTENSION__EXTENSION, oldExtension, newExtension);
            if (msgs == null) msgs = notification; else msgs.add(notification);
        }
        return msgs;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setExtension(TypeExtension newExtension) {
        if (newExtension != extension) {
            NotificationChain msgs = null;
            if (extension != null)
                msgs = ((InternalEObject)extension).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - Model2Package.FIELD_EXTENSION__EXTENSION, null, msgs);
            if (newExtension != null)
                msgs = ((InternalEObject)newExtension).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - Model2Package.FIELD_EXTENSION__EXTENSION, null, msgs);
            msgs = basicSetExtension(newExtension, msgs);
            if (msgs != null) msgs.dispatch();
        }
        else if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD_EXTENSION__EXTENSION, newExtension, newExtension));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.FIELD_EXTENSION__EXTENSION:
                return basicSetExtension(null, msgs);
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
            case Model2Package.FIELD_EXTENSION__EXTENSION:
                return getExtension();
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
            case Model2Package.FIELD_EXTENSION__EXTENSION:
                setExtension((TypeExtension)newValue);
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
            case Model2Package.FIELD_EXTENSION__EXTENSION:
                setExtension((TypeExtension)null);
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
            case Model2Package.FIELD_EXTENSION__EXTENSION:
                return extension != null;
        }
        return super.eIsSet(featureID);
    }

} //FieldExtensionImpl
