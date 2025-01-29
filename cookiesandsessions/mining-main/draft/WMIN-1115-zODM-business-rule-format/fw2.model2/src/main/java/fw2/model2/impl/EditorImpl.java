/**
 */
package fw2.model2.impl;

import fw2.model2.Editor;
import fw2.model2.FieldGroup;
import fw2.model2.Model2Package;
import fw2.model2.SummaryTable;

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
 * An implementation of the model object '<em><b>Editor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.EditorImpl#getEditors <em>Editors</em>}</li>
 *   <li>{@link fw2.model2.impl.EditorImpl#getFieldGroups <em>Field Groups</em>}</li>
 *   <li>{@link fw2.model2.impl.EditorImpl#getSummaryTables <em>Summary Tables</em>}</li>
 *   <li>{@link fw2.model2.impl.EditorImpl#getPresentInStaging <em>Present In Staging</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EditorImpl extends ViewComponentImpl implements Editor {
	/**
     * The cached value of the '{@link #getEditors() <em>Editors</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getEditors()
     * @generated
     * @ordered
     */
	protected EList<Editor> editors;

	/**
     * The cached value of the '{@link #getFieldGroups() <em>Field Groups</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getFieldGroups()
     * @generated
     * @ordered
     */
	protected EList<FieldGroup> fieldGroups;

	/**
     * The cached value of the '{@link #getSummaryTables() <em>Summary Tables</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSummaryTables()
     * @generated
     * @ordered
     */
	protected EList<SummaryTable> summaryTables;

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
	protected EditorImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.EDITOR;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Editor> getEditors() {
        if (editors == null) {
            editors = new EObjectContainmentEList<Editor>(Editor.class, this, Model2Package.EDITOR__EDITORS);
        }
        return editors;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<FieldGroup> getFieldGroups() {
        if (fieldGroups == null) {
            fieldGroups = new EObjectContainmentEList<FieldGroup>(FieldGroup.class, this, Model2Package.EDITOR__FIELD_GROUPS);
        }
        return fieldGroups;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<SummaryTable> getSummaryTables() {
        if (summaryTables == null) {
            summaryTables = new EObjectContainmentEList<SummaryTable>(SummaryTable.class, this, Model2Package.EDITOR__SUMMARY_TABLES);
        }
        return summaryTables;
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.EDITOR__PRESENT_IN_STAGING, oldPresentInStaging, presentInStaging));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.EDITOR__EDITORS:
                return ((InternalEList<?>)getEditors()).basicRemove(otherEnd, msgs);
            case Model2Package.EDITOR__FIELD_GROUPS:
                return ((InternalEList<?>)getFieldGroups()).basicRemove(otherEnd, msgs);
            case Model2Package.EDITOR__SUMMARY_TABLES:
                return ((InternalEList<?>)getSummaryTables()).basicRemove(otherEnd, msgs);
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
            case Model2Package.EDITOR__EDITORS:
                return getEditors();
            case Model2Package.EDITOR__FIELD_GROUPS:
                return getFieldGroups();
            case Model2Package.EDITOR__SUMMARY_TABLES:
                return getSummaryTables();
            case Model2Package.EDITOR__PRESENT_IN_STAGING:
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
            case Model2Package.EDITOR__EDITORS:
                getEditors().clear();
                getEditors().addAll((Collection<? extends Editor>)newValue);
                return;
            case Model2Package.EDITOR__FIELD_GROUPS:
                getFieldGroups().clear();
                getFieldGroups().addAll((Collection<? extends FieldGroup>)newValue);
                return;
            case Model2Package.EDITOR__SUMMARY_TABLES:
                getSummaryTables().clear();
                getSummaryTables().addAll((Collection<? extends SummaryTable>)newValue);
                return;
            case Model2Package.EDITOR__PRESENT_IN_STAGING:
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
            case Model2Package.EDITOR__EDITORS:
                getEditors().clear();
                return;
            case Model2Package.EDITOR__FIELD_GROUPS:
                getFieldGroups().clear();
                return;
            case Model2Package.EDITOR__SUMMARY_TABLES:
                getSummaryTables().clear();
                return;
            case Model2Package.EDITOR__PRESENT_IN_STAGING:
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
            case Model2Package.EDITOR__EDITORS:
                return editors != null && !editors.isEmpty();
            case Model2Package.EDITOR__FIELD_GROUPS:
                return fieldGroups != null && !fieldGroups.isEmpty();
            case Model2Package.EDITOR__SUMMARY_TABLES:
                return summaryTables != null && !summaryTables.isEmpty();
            case Model2Package.EDITOR__PRESENT_IN_STAGING:
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
        result.append(" (presentInStaging: ");
        result.append(presentInStaging);
        result.append(')');
        return result.toString();
    }

} //EditorImpl
