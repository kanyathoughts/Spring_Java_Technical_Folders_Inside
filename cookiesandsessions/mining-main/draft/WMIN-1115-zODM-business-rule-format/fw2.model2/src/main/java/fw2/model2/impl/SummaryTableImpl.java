/**
 */
package fw2.model2.impl;

import fw2.model2.Model2Package;
import fw2.model2.SummaryTable;
import fw2.model2.TableColumn;

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
 * An implementation of the model object '<em><b>Summary Table</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.SummaryTableImpl#getColumns <em>Columns</em>}</li>
 *   <li>{@link fw2.model2.impl.SummaryTableImpl#getDomainWrapper <em>Domain Wrapper</em>}</li>
 *   <li>{@link fw2.model2.impl.SummaryTableImpl#getBapName <em>Bap Name</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SummaryTableImpl extends ViewComponentImpl implements SummaryTable {
	/**
     * The cached value of the '{@link #getColumns() <em>Columns</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getColumns()
     * @generated
     * @ordered
     */
	protected EList<TableColumn> columns;

	/**
     * The default value of the '{@link #getDomainWrapper() <em>Domain Wrapper</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDomainWrapper()
     * @generated
     * @ordered
     */
	protected static final String DOMAIN_WRAPPER_EDEFAULT = null;
	/**
     * The cached value of the '{@link #getDomainWrapper() <em>Domain Wrapper</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDomainWrapper()
     * @generated
     * @ordered
     */
	protected String domainWrapper = DOMAIN_WRAPPER_EDEFAULT;

	/**
     * The default value of the '{@link #getBapName() <em>Bap Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getBapName()
     * @generated
     * @ordered
     */
	protected static final String BAP_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getBapName() <em>Bap Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getBapName()
     * @generated
     * @ordered
     */
	protected String bapName = BAP_NAME_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected SummaryTableImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.SUMMARY_TABLE;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<TableColumn> getColumns() {
        if (columns == null) {
            columns = new EObjectContainmentEList<TableColumn>(TableColumn.class, this, Model2Package.SUMMARY_TABLE__COLUMNS);
        }
        return columns;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getDomainWrapper() {
        return domainWrapper;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDomainWrapper(String newDomainWrapper) {
        String oldDomainWrapper = domainWrapper;
        domainWrapper = newDomainWrapper;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.SUMMARY_TABLE__DOMAIN_WRAPPER, oldDomainWrapper, domainWrapper));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getBapName() {
        return bapName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setBapName(String newBapName) {
        String oldBapName = bapName;
        bapName = newBapName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.SUMMARY_TABLE__BAP_NAME, oldBapName, bapName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.SUMMARY_TABLE__COLUMNS:
                return ((InternalEList<?>)getColumns()).basicRemove(otherEnd, msgs);
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
            case Model2Package.SUMMARY_TABLE__COLUMNS:
                return getColumns();
            case Model2Package.SUMMARY_TABLE__DOMAIN_WRAPPER:
                return getDomainWrapper();
            case Model2Package.SUMMARY_TABLE__BAP_NAME:
                return getBapName();
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
            case Model2Package.SUMMARY_TABLE__COLUMNS:
                getColumns().clear();
                getColumns().addAll((Collection<? extends TableColumn>)newValue);
                return;
            case Model2Package.SUMMARY_TABLE__DOMAIN_WRAPPER:
                setDomainWrapper((String)newValue);
                return;
            case Model2Package.SUMMARY_TABLE__BAP_NAME:
                setBapName((String)newValue);
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
            case Model2Package.SUMMARY_TABLE__COLUMNS:
                getColumns().clear();
                return;
            case Model2Package.SUMMARY_TABLE__DOMAIN_WRAPPER:
                setDomainWrapper(DOMAIN_WRAPPER_EDEFAULT);
                return;
            case Model2Package.SUMMARY_TABLE__BAP_NAME:
                setBapName(BAP_NAME_EDEFAULT);
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
            case Model2Package.SUMMARY_TABLE__COLUMNS:
                return columns != null && !columns.isEmpty();
            case Model2Package.SUMMARY_TABLE__DOMAIN_WRAPPER:
                return DOMAIN_WRAPPER_EDEFAULT == null ? domainWrapper != null : !DOMAIN_WRAPPER_EDEFAULT.equals(domainWrapper);
            case Model2Package.SUMMARY_TABLE__BAP_NAME:
                return BAP_NAME_EDEFAULT == null ? bapName != null : !BAP_NAME_EDEFAULT.equals(bapName);
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
        result.append(" (domainWrapper: ");
        result.append(domainWrapper);
        result.append(", bapName: ");
        result.append(bapName);
        result.append(')');
        return result.toString();
    }

} //SummaryTableImpl
