/**
 */
package fw2.model2.impl;

import fw2.model2.DbForeignKeyColumn;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Db Foreign Key Column</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DbForeignKeyColumnImpl#getKeySeq <em>Key Seq</em>}</li>
 *   <li>{@link fw2.model2.impl.DbForeignKeyColumnImpl#getSeq <em>Seq</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DbForeignKeyColumnImpl extends ModelElementImpl implements DbForeignKeyColumn {
	/**
     * The default value of the '{@link #getKeySeq() <em>Key Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getKeySeq()
     * @generated
     * @ordered
     */
	protected static final int KEY_SEQ_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getKeySeq() <em>Key Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getKeySeq()
     * @generated
     * @ordered
     */
	protected int keySeq = KEY_SEQ_EDEFAULT;

	/**
     * The default value of the '{@link #getSeq() <em>Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSeq()
     * @generated
     * @ordered
     */
	protected static final int SEQ_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getSeq() <em>Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSeq()
     * @generated
     * @ordered
     */
	protected int seq = SEQ_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DbForeignKeyColumnImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DB_FOREIGN_KEY_COLUMN;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getKeySeq() {
        return keySeq;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setKeySeq(int newKeySeq) {
        int oldKeySeq = keySeq;
        keySeq = newKeySeq;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_FOREIGN_KEY_COLUMN__KEY_SEQ, oldKeySeq, keySeq));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getSeq() {
        return seq;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setSeq(int newSeq) {
        int oldSeq = seq;
        seq = newSeq;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_FOREIGN_KEY_COLUMN__SEQ, oldSeq, seq));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.DB_FOREIGN_KEY_COLUMN__KEY_SEQ:
                return getKeySeq();
            case Model2Package.DB_FOREIGN_KEY_COLUMN__SEQ:
                return getSeq();
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
            case Model2Package.DB_FOREIGN_KEY_COLUMN__KEY_SEQ:
                setKeySeq((Integer)newValue);
                return;
            case Model2Package.DB_FOREIGN_KEY_COLUMN__SEQ:
                setSeq((Integer)newValue);
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
            case Model2Package.DB_FOREIGN_KEY_COLUMN__KEY_SEQ:
                setKeySeq(KEY_SEQ_EDEFAULT);
                return;
            case Model2Package.DB_FOREIGN_KEY_COLUMN__SEQ:
                setSeq(SEQ_EDEFAULT);
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
            case Model2Package.DB_FOREIGN_KEY_COLUMN__KEY_SEQ:
                return keySeq != KEY_SEQ_EDEFAULT;
            case Model2Package.DB_FOREIGN_KEY_COLUMN__SEQ:
                return seq != SEQ_EDEFAULT;
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
        result.append(" (keySeq: ");
        result.append(keySeq);
        result.append(", seq: ");
        result.append(seq);
        result.append(')');
        return result.toString();
    }

} //DbForeignKeyColumnImpl
