/**
 */
package fw2.model2.impl;

import fw2.model2.Application;
import fw2.model2.Bap;
import fw2.model2.Model2Package;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Application</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.ApplicationImpl#getBaps <em>Baps</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ApplicationImpl extends ViewComponentImpl implements Application {
	/**
     * The cached value of the '{@link #getBaps() <em>Baps</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getBaps()
     * @generated
     * @ordered
     */
	protected EList<Bap> baps;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ApplicationImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.APPLICATION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Bap> getBaps() {
        if (baps == null) {
            baps = new EObjectContainmentEList<Bap>(Bap.class, this, Model2Package.APPLICATION__BAPS);
        }
        return baps;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.APPLICATION__BAPS:
                return ((InternalEList<?>)getBaps()).basicRemove(otherEnd, msgs);
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
            case Model2Package.APPLICATION__BAPS:
                return getBaps();
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
            case Model2Package.APPLICATION__BAPS:
                getBaps().clear();
                getBaps().addAll((Collection<? extends Bap>)newValue);
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
            case Model2Package.APPLICATION__BAPS:
                getBaps().clear();
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
            case Model2Package.APPLICATION__BAPS:
                return baps != null && !baps.isEmpty();
        }
        return super.eIsSet(featureID);
    }

} //ApplicationImpl
