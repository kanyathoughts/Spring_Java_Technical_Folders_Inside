/**
 */
package fw2.model2.impl;

import fw2.model2.DbDataSet;
import fw2.model2.DbForeignKeyColumn;
import fw2.model2.DbRelation;
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
 * An implementation of the model object '<em><b>Db Relation</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DbRelationImpl#getForeignKeyColumns <em>Foreign Key Columns</em>}</li>
 *   <li>{@link fw2.model2.impl.DbRelationImpl#getRefTable <em>Ref Table</em>}</li>
 *   <li>{@link fw2.model2.impl.DbRelationImpl#getTable <em>Table</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DbRelationImpl extends ModelElementImpl implements DbRelation {
	/**
     * The cached value of the '{@link #getForeignKeyColumns() <em>Foreign Key Columns</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getForeignKeyColumns()
     * @generated
     * @ordered
     */
	protected EList<DbForeignKeyColumn> foreignKeyColumns;

	/**
     * The cached value of the '{@link #getRefTable() <em>Ref Table</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRefTable()
     * @generated
     * @ordered
     */
	protected DbDataSet refTable;

	/**
     * The cached value of the '{@link #getTable() <em>Table</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTable()
     * @generated
     * @ordered
     */
	protected DbDataSet table;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DbRelationImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DB_RELATION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<DbForeignKeyColumn> getForeignKeyColumns() {
        if (foreignKeyColumns == null) {
            foreignKeyColumns = new EObjectContainmentEList<DbForeignKeyColumn>(DbForeignKeyColumn.class, this, Model2Package.DB_RELATION__FOREIGN_KEY_COLUMNS);
        }
        return foreignKeyColumns;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbDataSet getRefTable() {
        return refTable;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setRefTable(DbDataSet newRefTable) {
        DbDataSet oldRefTable = refTable;
        refTable = newRefTable;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_RELATION__REF_TABLE, oldRefTable, refTable));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbDataSet getTable() {
        return table;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setTable(DbDataSet newTable) {
        DbDataSet oldTable = table;
        table = newTable;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_RELATION__TABLE, oldTable, table));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.DB_RELATION__FOREIGN_KEY_COLUMNS:
                return ((InternalEList<?>)getForeignKeyColumns()).basicRemove(otherEnd, msgs);
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
            case Model2Package.DB_RELATION__FOREIGN_KEY_COLUMNS:
                return getForeignKeyColumns();
            case Model2Package.DB_RELATION__REF_TABLE:
                return getRefTable();
            case Model2Package.DB_RELATION__TABLE:
                return getTable();
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
            case Model2Package.DB_RELATION__FOREIGN_KEY_COLUMNS:
                getForeignKeyColumns().clear();
                getForeignKeyColumns().addAll((Collection<? extends DbForeignKeyColumn>)newValue);
                return;
            case Model2Package.DB_RELATION__REF_TABLE:
                setRefTable((DbDataSet)newValue);
                return;
            case Model2Package.DB_RELATION__TABLE:
                setTable((DbDataSet)newValue);
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
            case Model2Package.DB_RELATION__FOREIGN_KEY_COLUMNS:
                getForeignKeyColumns().clear();
                return;
            case Model2Package.DB_RELATION__REF_TABLE:
                setRefTable((DbDataSet)null);
                return;
            case Model2Package.DB_RELATION__TABLE:
                setTable((DbDataSet)null);
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
            case Model2Package.DB_RELATION__FOREIGN_KEY_COLUMNS:
                return foreignKeyColumns != null && !foreignKeyColumns.isEmpty();
            case Model2Package.DB_RELATION__REF_TABLE:
                return refTable != null;
            case Model2Package.DB_RELATION__TABLE:
                return table != null;
        }
        return super.eIsSet(featureID);
    }

} //DbRelationImpl
