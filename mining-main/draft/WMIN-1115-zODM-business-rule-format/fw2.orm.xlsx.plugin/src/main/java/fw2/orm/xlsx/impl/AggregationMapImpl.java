/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.AggregationMap;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.XlsxPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Aggregation Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.impl.AggregationMapImpl#getComponentMap <em>Component Map</em>}</li>
 * </ul>
 *
 * @generated
 */
public class AggregationMapImpl extends AssociationMapImpl implements AggregationMap {
	/**
	 * The cached value of the '{@link #getComponentMap() <em>Component Map</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getComponentMap()
	 * @generated
	 * @ordered
	 */
	protected ComponentMap componentMap;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AggregationMapImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return XlsxPackage.Literals.AGGREGATION_MAP;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ComponentMap getComponentMap() {
		return componentMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetComponentMap(ComponentMap newComponentMap, NotificationChain msgs) {
		ComponentMap oldComponentMap = componentMap;
		componentMap = newComponentMap;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP, oldComponentMap, newComponentMap);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setComponentMap(ComponentMap newComponentMap) {
		if (newComponentMap != componentMap) {
			NotificationChain msgs = null;
			if (componentMap != null)
				msgs = ((InternalEObject)componentMap).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP, null, msgs);
			if (newComponentMap != null)
				msgs = ((InternalEObject)newComponentMap).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP, null, msgs);
			msgs = basicSetComponentMap(newComponentMap, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP, newComponentMap, newComponentMap));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP:
				return basicSetComponentMap(null, msgs);
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
			case XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP:
				return getComponentMap();
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
			case XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP:
				setComponentMap((ComponentMap)newValue);
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
			case XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP:
				setComponentMap((ComponentMap)null);
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
			case XlsxPackage.AGGREGATION_MAP__COMPONENT_MAP:
				return componentMap != null;
		}
		return super.eIsSet(featureID);
	}

} //AggregationMapImpl
