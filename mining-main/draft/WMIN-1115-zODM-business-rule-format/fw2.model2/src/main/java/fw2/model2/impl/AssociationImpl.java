/**
 */
package fw2.model2.impl;

import fw2.model2.Association;
import fw2.model2.CardinalityTypeEnum;
import fw2.model2.ClassOrm;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Association</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.AssociationImpl#getCardinality <em>Cardinality</em>}</li>
 *   <li>{@link fw2.model2.impl.AssociationImpl#getRelatedClassOrm <em>Related Class Orm</em>}</li>
 * </ul>
 *
 * @generated
 */
public abstract class AssociationImpl extends AttributeImpl implements Association {
	/**
     * The default value of the '{@link #getCardinality() <em>Cardinality</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getCardinality()
     * @generated
     * @ordered
     */
	protected static final CardinalityTypeEnum CARDINALITY_EDEFAULT = CardinalityTypeEnum.ONE;

	/**
     * The cached value of the '{@link #getCardinality() <em>Cardinality</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getCardinality()
     * @generated
     * @ordered
     */
	protected CardinalityTypeEnum cardinality = CARDINALITY_EDEFAULT;

	/**
     * The cached value of the '{@link #getRelatedClassOrm() <em>Related Class Orm</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRelatedClassOrm()
     * @generated
     * @ordered
     */
	protected ClassOrm relatedClassOrm;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected AssociationImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.ASSOCIATION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CardinalityTypeEnum getCardinality() {
        return cardinality;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setCardinality(CardinalityTypeEnum newCardinality) {
        CardinalityTypeEnum oldCardinality = cardinality;
        cardinality = newCardinality == null ? CARDINALITY_EDEFAULT : newCardinality;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.ASSOCIATION__CARDINALITY, oldCardinality, cardinality));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public ClassOrm getRelatedClassOrm() {
        if (relatedClassOrm != null && relatedClassOrm.eIsProxy()) {
            InternalEObject oldRelatedClassOrm = (InternalEObject)relatedClassOrm;
            relatedClassOrm = (ClassOrm)eResolveProxy(oldRelatedClassOrm);
            if (relatedClassOrm != oldRelatedClassOrm) {
                if (eNotificationRequired())
                    eNotify(new ENotificationImpl(this, Notification.RESOLVE, Model2Package.ASSOCIATION__RELATED_CLASS_ORM, oldRelatedClassOrm, relatedClassOrm));
            }
        }
        return relatedClassOrm;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public ClassOrm basicGetRelatedClassOrm() {
        return relatedClassOrm;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setRelatedClassOrm(ClassOrm newRelatedClassOrm) {
        ClassOrm oldRelatedClassOrm = relatedClassOrm;
        relatedClassOrm = newRelatedClassOrm;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.ASSOCIATION__RELATED_CLASS_ORM, oldRelatedClassOrm, relatedClassOrm));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.ASSOCIATION__CARDINALITY:
                return getCardinality();
            case Model2Package.ASSOCIATION__RELATED_CLASS_ORM:
                if (resolve) return getRelatedClassOrm();
                return basicGetRelatedClassOrm();
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
            case Model2Package.ASSOCIATION__CARDINALITY:
                setCardinality((CardinalityTypeEnum)newValue);
                return;
            case Model2Package.ASSOCIATION__RELATED_CLASS_ORM:
                setRelatedClassOrm((ClassOrm)newValue);
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
            case Model2Package.ASSOCIATION__CARDINALITY:
                setCardinality(CARDINALITY_EDEFAULT);
                return;
            case Model2Package.ASSOCIATION__RELATED_CLASS_ORM:
                setRelatedClassOrm((ClassOrm)null);
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
            case Model2Package.ASSOCIATION__CARDINALITY:
                return cardinality != CARDINALITY_EDEFAULT;
            case Model2Package.ASSOCIATION__RELATED_CLASS_ORM:
                return relatedClassOrm != null;
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
        result.append(" (cardinality: ");
        result.append(cardinality);
        result.append(')');
        return result.toString();
    }

} //AssociationImpl
