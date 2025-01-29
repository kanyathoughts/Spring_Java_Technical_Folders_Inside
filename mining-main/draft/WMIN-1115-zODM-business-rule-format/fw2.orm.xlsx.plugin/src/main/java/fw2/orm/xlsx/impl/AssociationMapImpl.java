/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.AssociationMap;
import fw2.orm.xlsx.CardinalityEnum;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.XlsxPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Association Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.impl.AssociationMapImpl#getCardinality <em>Cardinality</em>}</li>
 *   <li>{@link fw2.orm.xlsx.impl.AssociationMapImpl#getRelatedClassMap <em>Related Class Map</em>}</li>
 * </ul>
 *
 * @generated
 */
public abstract class AssociationMapImpl extends MapElementImpl implements AssociationMap {
	/**
	 * The default value of the '{@link #getCardinality() <em>Cardinality</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCardinality()
	 * @generated
	 * @ordered
	 */
	protected static final CardinalityEnum CARDINALITY_EDEFAULT = CardinalityEnum.ONE;

	/**
	 * The cached value of the '{@link #getCardinality() <em>Cardinality</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCardinality()
	 * @generated
	 * @ordered
	 */
	protected CardinalityEnum cardinality = CARDINALITY_EDEFAULT;

	/**
	 * The cached value of the '{@link #getRelatedClassMap() <em>Related Class Map</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRelatedClassMap()
	 * @generated
	 * @ordered
	 */
	protected ClassMap relatedClassMap;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AssociationMapImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return XlsxPackage.Literals.ASSOCIATION_MAP;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CardinalityEnum getCardinality() {
		return cardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCardinality(CardinalityEnum newCardinality) {
		CardinalityEnum oldCardinality = cardinality;
		cardinality = newCardinality == null ? CARDINALITY_EDEFAULT : newCardinality;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, XlsxPackage.ASSOCIATION_MAP__CARDINALITY, oldCardinality, cardinality));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassMap getRelatedClassMap() {
		if (relatedClassMap != null && relatedClassMap.eIsProxy()) {
			InternalEObject oldRelatedClassMap = (InternalEObject)relatedClassMap;
			relatedClassMap = (ClassMap)eResolveProxy(oldRelatedClassMap);
			if (relatedClassMap != oldRelatedClassMap) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, XlsxPackage.ASSOCIATION_MAP__RELATED_CLASS_MAP, oldRelatedClassMap, relatedClassMap));
			}
		}
		return relatedClassMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassMap basicGetRelatedClassMap() {
		return relatedClassMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRelatedClassMap(ClassMap newRelatedClassMap) {
		ClassMap oldRelatedClassMap = relatedClassMap;
		relatedClassMap = newRelatedClassMap;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, XlsxPackage.ASSOCIATION_MAP__RELATED_CLASS_MAP, oldRelatedClassMap, relatedClassMap));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case XlsxPackage.ASSOCIATION_MAP__CARDINALITY:
				return getCardinality();
			case XlsxPackage.ASSOCIATION_MAP__RELATED_CLASS_MAP:
				if (resolve) return getRelatedClassMap();
				return basicGetRelatedClassMap();
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
			case XlsxPackage.ASSOCIATION_MAP__CARDINALITY:
				setCardinality((CardinalityEnum)newValue);
				return;
			case XlsxPackage.ASSOCIATION_MAP__RELATED_CLASS_MAP:
				setRelatedClassMap((ClassMap)newValue);
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
			case XlsxPackage.ASSOCIATION_MAP__CARDINALITY:
				setCardinality(CARDINALITY_EDEFAULT);
				return;
			case XlsxPackage.ASSOCIATION_MAP__RELATED_CLASS_MAP:
				setRelatedClassMap((ClassMap)null);
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
			case XlsxPackage.ASSOCIATION_MAP__CARDINALITY:
				return cardinality != CARDINALITY_EDEFAULT;
			case XlsxPackage.ASSOCIATION_MAP__RELATED_CLASS_MAP:
				return relatedClassMap != null;
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

} //AssociationMapImpl
