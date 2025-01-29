/**
 */
package fw2.model2.impl;

import fw2.model2.Component;
import fw2.model2.Lookup;
import fw2.model2.Model2Package;
import fw2.model2.Primitive;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Component</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.ComponentImpl#getLookups <em>Lookups</em>}</li>
 *   <li>{@link fw2.model2.impl.ComponentImpl#getPrimitives <em>Primitives</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ComponentImpl extends ModelElementImpl implements Component {
	/**
     * The cached value of the '{@link #getLookups() <em>Lookups</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getLookups()
     * @generated
     * @ordered
     */
	protected EList<Lookup> lookups;

	/**
     * The cached value of the '{@link #getPrimitives() <em>Primitives</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPrimitives()
     * @generated
     * @ordered
     */
	protected EList<Primitive> primitives;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ComponentImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.COMPONENT;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Lookup> getLookups() {
        if (lookups == null) {
            lookups = new EObjectContainmentEList<Lookup>(Lookup.class, this, Model2Package.COMPONENT__LOOKUPS);
        }
        return lookups;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Primitive> getPrimitives() {
        if (primitives == null) {
            primitives = new EObjectContainmentEList<Primitive>(Primitive.class, this, Model2Package.COMPONENT__PRIMITIVES);
        }
        return primitives;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.COMPONENT__LOOKUPS:
                return ((InternalEList<?>)getLookups()).basicRemove(otherEnd, msgs);
            case Model2Package.COMPONENT__PRIMITIVES:
                return ((InternalEList<?>)getPrimitives()).basicRemove(otherEnd, msgs);
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
            case Model2Package.COMPONENT__LOOKUPS:
                return getLookups();
            case Model2Package.COMPONENT__PRIMITIVES:
                return getPrimitives();
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
            case Model2Package.COMPONENT__LOOKUPS:
                getLookups().clear();
                getLookups().addAll((Collection<? extends Lookup>)newValue);
                return;
            case Model2Package.COMPONENT__PRIMITIVES:
                getPrimitives().clear();
                getPrimitives().addAll((Collection<? extends Primitive>)newValue);
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
            case Model2Package.COMPONENT__LOOKUPS:
                getLookups().clear();
                return;
            case Model2Package.COMPONENT__PRIMITIVES:
                getPrimitives().clear();
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
            case Model2Package.COMPONENT__LOOKUPS:
                return lookups != null && !lookups.isEmpty();
            case Model2Package.COMPONENT__PRIMITIVES:
                return primitives != null && !primitives.isEmpty();
        }
        return super.eIsSet(featureID);
    }

} //ComponentImpl
