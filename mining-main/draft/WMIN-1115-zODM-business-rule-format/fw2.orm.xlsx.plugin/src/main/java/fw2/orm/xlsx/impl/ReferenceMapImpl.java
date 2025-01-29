/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.ReferenceKey;
import fw2.orm.xlsx.ReferenceMap;
import fw2.orm.xlsx.XlsxPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Reference Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.impl.ReferenceMapImpl#getReferenceKeys <em>Reference Keys</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ReferenceMapImpl extends AssociationMapImpl implements ReferenceMap {
	/**
	 * The cached value of the '{@link #getReferenceKeys() <em>Reference Keys</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReferenceKeys()
	 * @generated
	 * @ordered
	 */
	protected EList<ReferenceKey> referenceKeys;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ReferenceMapImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return XlsxPackage.Literals.REFERENCE_MAP;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ReferenceKey> getReferenceKeys() {
		if (referenceKeys == null) {
			referenceKeys = new EObjectContainmentEList<ReferenceKey>(ReferenceKey.class, this, XlsxPackage.REFERENCE_MAP__REFERENCE_KEYS);
		}
		return referenceKeys;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case XlsxPackage.REFERENCE_MAP__REFERENCE_KEYS:
				return ((InternalEList<?>)getReferenceKeys()).basicRemove(otherEnd, msgs);
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
			case XlsxPackage.REFERENCE_MAP__REFERENCE_KEYS:
				return getReferenceKeys();
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
			case XlsxPackage.REFERENCE_MAP__REFERENCE_KEYS:
				getReferenceKeys().clear();
				getReferenceKeys().addAll((Collection<? extends ReferenceKey>)newValue);
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
			case XlsxPackage.REFERENCE_MAP__REFERENCE_KEYS:
				getReferenceKeys().clear();
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
			case XlsxPackage.REFERENCE_MAP__REFERENCE_KEYS:
				return referenceKeys != null && !referenceKeys.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //ReferenceMapImpl
