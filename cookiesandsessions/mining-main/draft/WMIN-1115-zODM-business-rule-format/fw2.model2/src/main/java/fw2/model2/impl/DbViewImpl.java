/**
 */
package fw2.model2.impl;

import fw2.model2.DbJoinKey;
import fw2.model2.DbJoinParameter;
import fw2.model2.DbView;
import fw2.model2.DbViewColumn;
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
 * An implementation of the model object '<em><b>Db View</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DbViewImpl#getJoinKeys <em>Join Keys</em>}</li>
 *   <li>{@link fw2.model2.impl.DbViewImpl#getJoinParameters <em>Join Parameters</em>}</li>
 *   <li>{@link fw2.model2.impl.DbViewImpl#getViewColumns <em>View Columns</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DbViewImpl extends DbDataSetImpl implements DbView {
	/**
     * The cached value of the '{@link #getJoinKeys() <em>Join Keys</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getJoinKeys()
     * @generated
     * @ordered
     */
	protected EList<DbJoinKey> joinKeys;

	/**
     * The cached value of the '{@link #getJoinParameters() <em>Join Parameters</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getJoinParameters()
     * @generated
     * @ordered
     */
	protected EList<DbJoinParameter> joinParameters;

	/**
     * The cached value of the '{@link #getViewColumns() <em>View Columns</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getViewColumns()
     * @generated
     * @ordered
     */
	protected EList<DbViewColumn> viewColumns;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DbViewImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DB_VIEW;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<DbJoinKey> getJoinKeys() {
        if (joinKeys == null) {
            joinKeys = new EObjectContainmentEList<DbJoinKey>(DbJoinKey.class, this, Model2Package.DB_VIEW__JOIN_KEYS);
        }
        return joinKeys;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<DbJoinParameter> getJoinParameters() {
        if (joinParameters == null) {
            joinParameters = new EObjectContainmentEList<DbJoinParameter>(DbJoinParameter.class, this, Model2Package.DB_VIEW__JOIN_PARAMETERS);
        }
        return joinParameters;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<DbViewColumn> getViewColumns() {
        if (viewColumns == null) {
            viewColumns = new EObjectContainmentEList<DbViewColumn>(DbViewColumn.class, this, Model2Package.DB_VIEW__VIEW_COLUMNS);
        }
        return viewColumns;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.DB_VIEW__JOIN_KEYS:
                return ((InternalEList<?>)getJoinKeys()).basicRemove(otherEnd, msgs);
            case Model2Package.DB_VIEW__JOIN_PARAMETERS:
                return ((InternalEList<?>)getJoinParameters()).basicRemove(otherEnd, msgs);
            case Model2Package.DB_VIEW__VIEW_COLUMNS:
                return ((InternalEList<?>)getViewColumns()).basicRemove(otherEnd, msgs);
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
            case Model2Package.DB_VIEW__JOIN_KEYS:
                return getJoinKeys();
            case Model2Package.DB_VIEW__JOIN_PARAMETERS:
                return getJoinParameters();
            case Model2Package.DB_VIEW__VIEW_COLUMNS:
                return getViewColumns();
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
            case Model2Package.DB_VIEW__JOIN_KEYS:
                getJoinKeys().clear();
                getJoinKeys().addAll((Collection<? extends DbJoinKey>)newValue);
                return;
            case Model2Package.DB_VIEW__JOIN_PARAMETERS:
                getJoinParameters().clear();
                getJoinParameters().addAll((Collection<? extends DbJoinParameter>)newValue);
                return;
            case Model2Package.DB_VIEW__VIEW_COLUMNS:
                getViewColumns().clear();
                getViewColumns().addAll((Collection<? extends DbViewColumn>)newValue);
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
            case Model2Package.DB_VIEW__JOIN_KEYS:
                getJoinKeys().clear();
                return;
            case Model2Package.DB_VIEW__JOIN_PARAMETERS:
                getJoinParameters().clear();
                return;
            case Model2Package.DB_VIEW__VIEW_COLUMNS:
                getViewColumns().clear();
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
            case Model2Package.DB_VIEW__JOIN_KEYS:
                return joinKeys != null && !joinKeys.isEmpty();
            case Model2Package.DB_VIEW__JOIN_PARAMETERS:
                return joinParameters != null && !joinParameters.isEmpty();
            case Model2Package.DB_VIEW__VIEW_COLUMNS:
                return viewColumns != null && !viewColumns.isEmpty();
        }
        return super.eIsSet(featureID);
    }

} //DbViewImpl
