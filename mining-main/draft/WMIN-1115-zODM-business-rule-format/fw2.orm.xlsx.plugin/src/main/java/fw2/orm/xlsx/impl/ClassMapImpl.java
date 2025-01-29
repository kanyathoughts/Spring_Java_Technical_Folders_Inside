/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.AggregationMap;
import fw2.orm.xlsx.ClassMap;
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
 * An implementation of the model object '<em><b>Class Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.impl.ClassMapImpl#getAggregationMaps <em>Aggregation Maps</em>}</li>
 *   <li>{@link fw2.orm.xlsx.impl.ClassMapImpl#getReferenceMaps <em>Reference Maps</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ClassMapImpl extends ComponentMapImpl implements ClassMap {
	/**
	 * The cached value of the '{@link #getAggregationMaps() <em>Aggregation Maps</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAggregationMaps()
	 * @generated
	 * @ordered
	 */
	protected EList<AggregationMap> aggregationMaps;

	/**
	 * The cached value of the '{@link #getReferenceMaps() <em>Reference Maps</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReferenceMaps()
	 * @generated
	 * @ordered
	 */
	protected EList<ReferenceMap> referenceMaps;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ClassMapImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return XlsxPackage.Literals.CLASS_MAP;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<AggregationMap> getAggregationMaps() {
		if (aggregationMaps == null) {
			aggregationMaps = new EObjectContainmentEList<AggregationMap>(AggregationMap.class, this, XlsxPackage.CLASS_MAP__AGGREGATION_MAPS);
		}
		return aggregationMaps;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ReferenceMap> getReferenceMaps() {
		if (referenceMaps == null) {
			referenceMaps = new EObjectContainmentEList<ReferenceMap>(ReferenceMap.class, this, XlsxPackage.CLASS_MAP__REFERENCE_MAPS);
		}
		return referenceMaps;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case XlsxPackage.CLASS_MAP__AGGREGATION_MAPS:
				return ((InternalEList<?>)getAggregationMaps()).basicRemove(otherEnd, msgs);
			case XlsxPackage.CLASS_MAP__REFERENCE_MAPS:
				return ((InternalEList<?>)getReferenceMaps()).basicRemove(otherEnd, msgs);
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
			case XlsxPackage.CLASS_MAP__AGGREGATION_MAPS:
				return getAggregationMaps();
			case XlsxPackage.CLASS_MAP__REFERENCE_MAPS:
				return getReferenceMaps();
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
			case XlsxPackage.CLASS_MAP__AGGREGATION_MAPS:
				getAggregationMaps().clear();
				getAggregationMaps().addAll((Collection<? extends AggregationMap>)newValue);
				return;
			case XlsxPackage.CLASS_MAP__REFERENCE_MAPS:
				getReferenceMaps().clear();
				getReferenceMaps().addAll((Collection<? extends ReferenceMap>)newValue);
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
			case XlsxPackage.CLASS_MAP__AGGREGATION_MAPS:
				getAggregationMaps().clear();
				return;
			case XlsxPackage.CLASS_MAP__REFERENCE_MAPS:
				getReferenceMaps().clear();
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
			case XlsxPackage.CLASS_MAP__AGGREGATION_MAPS:
				return aggregationMaps != null && !aggregationMaps.isEmpty();
			case XlsxPackage.CLASS_MAP__REFERENCE_MAPS:
				return referenceMaps != null && !referenceMaps.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //ClassMapImpl
