/**
 */
package fw2.model2.impl;

import fw2.model2.Aggregation;
import fw2.model2.Component;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Aggregation</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.AggregationImpl#getComponent <em>Component</em>}</li>
 * </ul>
 *
 * @generated
 */
public class AggregationImpl extends AssociationImpl implements Aggregation {
	/**
     * The cached value of the '{@link #getComponent() <em>Component</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getComponent()
     * @generated
     * @ordered
     */
	protected Component component;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected AggregationImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.AGGREGATION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Component getComponent() {
        return component;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public NotificationChain basicSetComponent(Component newComponent, NotificationChain msgs) {
        Component oldComponent = component;
        component = newComponent;
        if (eNotificationRequired()) {
            ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, Model2Package.AGGREGATION__COMPONENT, oldComponent, newComponent);
            if (msgs == null) msgs = notification; else msgs.add(notification);
        }
        return msgs;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setComponent(Component newComponent) {
        if (newComponent != component) {
            NotificationChain msgs = null;
            if (component != null)
                msgs = ((InternalEObject)component).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - Model2Package.AGGREGATION__COMPONENT, null, msgs);
            if (newComponent != null)
                msgs = ((InternalEObject)newComponent).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - Model2Package.AGGREGATION__COMPONENT, null, msgs);
            msgs = basicSetComponent(newComponent, msgs);
            if (msgs != null) msgs.dispatch();
        }
        else if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.AGGREGATION__COMPONENT, newComponent, newComponent));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.AGGREGATION__COMPONENT:
                return basicSetComponent(null, msgs);
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
            case Model2Package.AGGREGATION__COMPONENT:
                return getComponent();
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
            case Model2Package.AGGREGATION__COMPONENT:
                setComponent((Component)newValue);
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
            case Model2Package.AGGREGATION__COMPONENT:
                setComponent((Component)null);
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
            case Model2Package.AGGREGATION__COMPONENT:
                return component != null;
        }
        return super.eIsSet(featureID);
    }

} //AggregationImpl
