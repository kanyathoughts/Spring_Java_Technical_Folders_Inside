/**
 */
package fw2.model2.impl;

import fw2.model2.DbRelation;
import fw2.model2.Model2Package;
import fw2.model2.Reference;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Reference</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.ReferenceImpl#getRelation <em>Relation</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ReferenceImpl extends AssociationImpl implements Reference {
	/**
     * The cached value of the '{@link #getRelation() <em>Relation</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRelation()
     * @generated
     * @ordered
     */
	protected DbRelation relation;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ReferenceImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.REFERENCE;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbRelation getRelation() {
        return relation;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setRelation(DbRelation newRelation) {
        DbRelation oldRelation = relation;
        relation = newRelation;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.REFERENCE__RELATION, oldRelation, relation));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.REFERENCE__RELATION:
                return getRelation();
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
            case Model2Package.REFERENCE__RELATION:
                setRelation((DbRelation)newValue);
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
            case Model2Package.REFERENCE__RELATION:
                setRelation((DbRelation)null);
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
            case Model2Package.REFERENCE__RELATION:
                return relation != null;
        }
        return super.eIsSet(featureID);
    }

} //ReferenceImpl
