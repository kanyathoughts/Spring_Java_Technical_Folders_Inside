/**
 */
package fw2.model2.impl;

import fw2.model2.Action;
import fw2.model2.Field;
import fw2.model2.FieldGroup;
import fw2.model2.Model2Package;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Field Group</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.FieldGroupImpl#getActions <em>Actions</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldGroupImpl#getFields <em>Fields</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldGroupImpl#getDisplayOrder <em>Display Order</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldGroupImpl#getPresentInStaging <em>Present In Staging</em>}</li>
 * </ul>
 *
 * @generated
 */
public class FieldGroupImpl extends ViewComponentImpl implements FieldGroup {
	/**
     * The cached value of the '{@link #getActions() <em>Actions</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getActions()
     * @generated
     * @ordered
     */
	protected EList<Action> actions;

	/**
     * The cached value of the '{@link #getFields() <em>Fields</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getFields()
     * @generated
     * @ordered
     */
	protected EList<Field> fields;

	/**
     * The default value of the '{@link #getDisplayOrder() <em>Display Order</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDisplayOrder()
     * @generated
     * @ordered
     */
	protected static final int DISPLAY_ORDER_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getDisplayOrder() <em>Display Order</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDisplayOrder()
     * @generated
     * @ordered
     */
	protected int displayOrder = DISPLAY_ORDER_EDEFAULT;

	/**
     * The default value of the '{@link #getPresentInStaging() <em>Present In Staging</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPresentInStaging()
     * @generated
     * @ordered
     */
	protected static final String PRESENT_IN_STAGING_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getPresentInStaging() <em>Present In Staging</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPresentInStaging()
     * @generated
     * @ordered
     */
	protected String presentInStaging = PRESENT_IN_STAGING_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected FieldGroupImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.FIELD_GROUP;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Action> getActions() {
        if (actions == null) {
            actions = new EObjectContainmentEList<Action>(Action.class, this, Model2Package.FIELD_GROUP__ACTIONS);
        }
        return actions;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Field> getFields() {
        if (fields == null) {
            fields = new EObjectContainmentEList<Field>(Field.class, this, Model2Package.FIELD_GROUP__FIELDS);
        }
        return fields;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getDisplayOrder() {
        return displayOrder;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDisplayOrder(int newDisplayOrder) {
        int oldDisplayOrder = displayOrder;
        displayOrder = newDisplayOrder;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD_GROUP__DISPLAY_ORDER, oldDisplayOrder, displayOrder));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getPresentInStaging() {
        return presentInStaging;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPresentInStaging(String newPresentInStaging) {
        String oldPresentInStaging = presentInStaging;
        presentInStaging = newPresentInStaging;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD_GROUP__PRESENT_IN_STAGING, oldPresentInStaging, presentInStaging));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.FIELD_GROUP__ACTIONS:
                return ((InternalEList<?>)getActions()).basicRemove(otherEnd, msgs);
            case Model2Package.FIELD_GROUP__FIELDS:
                return ((InternalEList<?>)getFields()).basicRemove(otherEnd, msgs);
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
            case Model2Package.FIELD_GROUP__ACTIONS:
                return getActions();
            case Model2Package.FIELD_GROUP__FIELDS:
                return getFields();
            case Model2Package.FIELD_GROUP__DISPLAY_ORDER:
                return getDisplayOrder();
            case Model2Package.FIELD_GROUP__PRESENT_IN_STAGING:
                return getPresentInStaging();
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
            case Model2Package.FIELD_GROUP__ACTIONS:
                getActions().clear();
                getActions().addAll((Collection<? extends Action>)newValue);
                return;
            case Model2Package.FIELD_GROUP__FIELDS:
                getFields().clear();
                getFields().addAll((Collection<? extends Field>)newValue);
                return;
            case Model2Package.FIELD_GROUP__DISPLAY_ORDER:
                setDisplayOrder((Integer)newValue);
                return;
            case Model2Package.FIELD_GROUP__PRESENT_IN_STAGING:
                setPresentInStaging((String)newValue);
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
            case Model2Package.FIELD_GROUP__ACTIONS:
                getActions().clear();
                return;
            case Model2Package.FIELD_GROUP__FIELDS:
                getFields().clear();
                return;
            case Model2Package.FIELD_GROUP__DISPLAY_ORDER:
                setDisplayOrder(DISPLAY_ORDER_EDEFAULT);
                return;
            case Model2Package.FIELD_GROUP__PRESENT_IN_STAGING:
                setPresentInStaging(PRESENT_IN_STAGING_EDEFAULT);
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
            case Model2Package.FIELD_GROUP__ACTIONS:
                return actions != null && !actions.isEmpty();
            case Model2Package.FIELD_GROUP__FIELDS:
                return fields != null && !fields.isEmpty();
            case Model2Package.FIELD_GROUP__DISPLAY_ORDER:
                return displayOrder != DISPLAY_ORDER_EDEFAULT;
            case Model2Package.FIELD_GROUP__PRESENT_IN_STAGING:
                return PRESENT_IN_STAGING_EDEFAULT == null ? presentInStaging != null : !PRESENT_IN_STAGING_EDEFAULT.equals(presentInStaging);
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
        result.append(" (displayOrder: ");
        result.append(displayOrder);
        result.append(", presentInStaging: ");
        result.append(presentInStaging);
        result.append(')');
        return result.toString();
    }

} //FieldGroupImpl
