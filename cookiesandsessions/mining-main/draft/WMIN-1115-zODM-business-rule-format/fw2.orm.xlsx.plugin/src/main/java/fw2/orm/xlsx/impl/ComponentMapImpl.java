/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.LookupMap;
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
 * An implementation of the model object '<em><b>Component Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.impl.ComponentMapImpl#getColumnMaps <em>Column Maps</em>}</li>
 *   <li>{@link fw2.orm.xlsx.impl.ComponentMapImpl#getLookupMaps <em>Lookup Maps</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ComponentMapImpl extends MapElementImpl implements ComponentMap {
	/**
	 * The cached value of the '{@link #getColumnMaps() <em>Column Maps</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getColumnMaps()
	 * @generated
	 * @ordered
	 */
	protected EList<ColumnMap> columnMaps;

	/**
	 * The cached value of the '{@link #getLookupMaps() <em>Lookup Maps</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLookupMaps()
	 * @generated
	 * @ordered
	 */
	protected EList<LookupMap> lookupMaps;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ComponentMapImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return XlsxPackage.Literals.COMPONENT_MAP;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ColumnMap> getColumnMaps() {
		if (columnMaps == null) {
			columnMaps = new EObjectContainmentEList<ColumnMap>(ColumnMap.class, this, XlsxPackage.COMPONENT_MAP__COLUMN_MAPS);
		}
		return columnMaps;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<LookupMap> getLookupMaps() {
		if (lookupMaps == null) {
			lookupMaps = new EObjectContainmentEList<LookupMap>(LookupMap.class, this, XlsxPackage.COMPONENT_MAP__LOOKUP_MAPS);
		}
		return lookupMaps;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case XlsxPackage.COMPONENT_MAP__COLUMN_MAPS:
				return ((InternalEList<?>)getColumnMaps()).basicRemove(otherEnd, msgs);
			case XlsxPackage.COMPONENT_MAP__LOOKUP_MAPS:
				return ((InternalEList<?>)getLookupMaps()).basicRemove(otherEnd, msgs);
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
			case XlsxPackage.COMPONENT_MAP__COLUMN_MAPS:
				return getColumnMaps();
			case XlsxPackage.COMPONENT_MAP__LOOKUP_MAPS:
				return getLookupMaps();
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
			case XlsxPackage.COMPONENT_MAP__COLUMN_MAPS:
				getColumnMaps().clear();
				getColumnMaps().addAll((Collection<? extends ColumnMap>)newValue);
				return;
			case XlsxPackage.COMPONENT_MAP__LOOKUP_MAPS:
				getLookupMaps().clear();
				getLookupMaps().addAll((Collection<? extends LookupMap>)newValue);
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
			case XlsxPackage.COMPONENT_MAP__COLUMN_MAPS:
				getColumnMaps().clear();
				return;
			case XlsxPackage.COMPONENT_MAP__LOOKUP_MAPS:
				getLookupMaps().clear();
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
			case XlsxPackage.COMPONENT_MAP__COLUMN_MAPS:
				return columnMaps != null && !columnMaps.isEmpty();
			case XlsxPackage.COMPONENT_MAP__LOOKUP_MAPS:
				return lookupMaps != null && !lookupMaps.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //ComponentMapImpl
