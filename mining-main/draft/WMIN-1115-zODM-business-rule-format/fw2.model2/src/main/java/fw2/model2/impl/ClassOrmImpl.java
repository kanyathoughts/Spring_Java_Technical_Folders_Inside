/**
 */
package fw2.model2.impl;

import fw2.model2.Aggregation;
import fw2.model2.ClassOrm;
import fw2.model2.DbDataSet;
import fw2.model2.DiscriminatorKey;
import fw2.model2.Model2Package;
import fw2.model2.Reference;

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
 * An implementation of the model object '<em><b>Class Orm</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.ClassOrmImpl#getAggregations <em>Aggregations</em>}</li>
 *   <li>{@link fw2.model2.impl.ClassOrmImpl#getDataSet <em>Data Set</em>}</li>
 *   <li>{@link fw2.model2.impl.ClassOrmImpl#getKey <em>Key</em>}</li>
 *   <li>{@link fw2.model2.impl.ClassOrmImpl#getReferences <em>References</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ClassOrmImpl extends ComponentImpl implements ClassOrm {
	/**
     * The cached value of the '{@link #getAggregations() <em>Aggregations</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getAggregations()
     * @generated
     * @ordered
     */
	protected EList<Aggregation> aggregations;

	/**
     * The cached value of the '{@link #getDataSet() <em>Data Set</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDataSet()
     * @generated
     * @ordered
     */
	protected DbDataSet dataSet;

	/**
     * The cached value of the '{@link #getKey() <em>Key</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getKey()
     * @generated
     * @ordered
     */
	protected DiscriminatorKey key;

	/**
     * The cached value of the '{@link #getReferences() <em>References</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getReferences()
     * @generated
     * @ordered
     */
	protected EList<Reference> references;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ClassOrmImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.CLASS_ORM;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Aggregation> getAggregations() {
        if (aggregations == null) {
            aggregations = new EObjectContainmentEList<Aggregation>(Aggregation.class, this, Model2Package.CLASS_ORM__AGGREGATIONS);
        }
        return aggregations;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbDataSet getDataSet() {
        return dataSet;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDataSet(DbDataSet newDataSet) {
        DbDataSet oldDataSet = dataSet;
        dataSet = newDataSet;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.CLASS_ORM__DATA_SET, oldDataSet, dataSet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DiscriminatorKey getKey() {
        return key;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public NotificationChain basicSetKey(DiscriminatorKey newKey, NotificationChain msgs) {
        DiscriminatorKey oldKey = key;
        key = newKey;
        if (eNotificationRequired()) {
            ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, Model2Package.CLASS_ORM__KEY, oldKey, newKey);
            if (msgs == null) msgs = notification; else msgs.add(notification);
        }
        return msgs;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setKey(DiscriminatorKey newKey) {
        if (newKey != key) {
            NotificationChain msgs = null;
            if (key != null)
                msgs = ((InternalEObject)key).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - Model2Package.CLASS_ORM__KEY, null, msgs);
            if (newKey != null)
                msgs = ((InternalEObject)newKey).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - Model2Package.CLASS_ORM__KEY, null, msgs);
            msgs = basicSetKey(newKey, msgs);
            if (msgs != null) msgs.dispatch();
        }
        else if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.CLASS_ORM__KEY, newKey, newKey));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Reference> getReferences() {
        if (references == null) {
            references = new EObjectContainmentEList<Reference>(Reference.class, this, Model2Package.CLASS_ORM__REFERENCES);
        }
        return references;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.CLASS_ORM__AGGREGATIONS:
                return ((InternalEList<?>)getAggregations()).basicRemove(otherEnd, msgs);
            case Model2Package.CLASS_ORM__KEY:
                return basicSetKey(null, msgs);
            case Model2Package.CLASS_ORM__REFERENCES:
                return ((InternalEList<?>)getReferences()).basicRemove(otherEnd, msgs);
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
            case Model2Package.CLASS_ORM__AGGREGATIONS:
                return getAggregations();
            case Model2Package.CLASS_ORM__DATA_SET:
                return getDataSet();
            case Model2Package.CLASS_ORM__KEY:
                return getKey();
            case Model2Package.CLASS_ORM__REFERENCES:
                return getReferences();
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
            case Model2Package.CLASS_ORM__AGGREGATIONS:
                getAggregations().clear();
                getAggregations().addAll((Collection<? extends Aggregation>)newValue);
                return;
            case Model2Package.CLASS_ORM__DATA_SET:
                setDataSet((DbDataSet)newValue);
                return;
            case Model2Package.CLASS_ORM__KEY:
                setKey((DiscriminatorKey)newValue);
                return;
            case Model2Package.CLASS_ORM__REFERENCES:
                getReferences().clear();
                getReferences().addAll((Collection<? extends Reference>)newValue);
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
            case Model2Package.CLASS_ORM__AGGREGATIONS:
                getAggregations().clear();
                return;
            case Model2Package.CLASS_ORM__DATA_SET:
                setDataSet((DbDataSet)null);
                return;
            case Model2Package.CLASS_ORM__KEY:
                setKey((DiscriminatorKey)null);
                return;
            case Model2Package.CLASS_ORM__REFERENCES:
                getReferences().clear();
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
            case Model2Package.CLASS_ORM__AGGREGATIONS:
                return aggregations != null && !aggregations.isEmpty();
            case Model2Package.CLASS_ORM__DATA_SET:
                return dataSet != null;
            case Model2Package.CLASS_ORM__KEY:
                return key != null;
            case Model2Package.CLASS_ORM__REFERENCES:
                return references != null && !references.isEmpty();
        }
        return super.eIsSet(featureID);
    }

} //ClassOrmImpl
