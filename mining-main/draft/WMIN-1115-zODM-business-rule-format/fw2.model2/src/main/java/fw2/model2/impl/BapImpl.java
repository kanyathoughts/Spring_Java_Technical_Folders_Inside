/**
 */
package fw2.model2.impl;

import fw2.model2.Bap;
import fw2.model2.ComponentMapping;
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
 * An implementation of the model object '<em><b>Bap</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.BapImpl#getComponentMapping <em>Component Mapping</em>}</li>
 * </ul>
 *
 * @generated
 */
public class BapImpl extends EditorImpl implements Bap {
	/**
     * The cached value of the '{@link #getComponentMapping() <em>Component Mapping</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getComponentMapping()
     * @generated
     * @ordered
     */
	protected EList<ComponentMapping> componentMapping;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected BapImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.BAP;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<ComponentMapping> getComponentMapping() {
        if (componentMapping == null) {
            componentMapping = new EObjectContainmentEList<ComponentMapping>(ComponentMapping.class, this, Model2Package.BAP__COMPONENT_MAPPING);
        }
        return componentMapping;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.BAP__COMPONENT_MAPPING:
                return ((InternalEList<?>)getComponentMapping()).basicRemove(otherEnd, msgs);
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
            case Model2Package.BAP__COMPONENT_MAPPING:
                return getComponentMapping();
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
            case Model2Package.BAP__COMPONENT_MAPPING:
                getComponentMapping().clear();
                getComponentMapping().addAll((Collection<? extends ComponentMapping>)newValue);
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
            case Model2Package.BAP__COMPONENT_MAPPING:
                getComponentMapping().clear();
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
            case Model2Package.BAP__COMPONENT_MAPPING:
                return componentMapping != null && !componentMapping.isEmpty();
        }
        return super.eIsSet(featureID);
    }

} //BapImpl
