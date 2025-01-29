/**
 */
package fw2.model2.impl;

import fw2.model2.DbColumn;
import fw2.model2.DbColumnTypeEnum;
import fw2.model2.DbTable;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Db Column</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getDefault <em>Default</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getKeySeq <em>Key Seq</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getLength <em>Length</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getNullable <em>Nullable</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getScale <em>Scale</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getSeq <em>Seq</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getSize <em>Size</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getType <em>Type</em>}</li>
 *   <li>{@link fw2.model2.impl.DbColumnImpl#getTable <em>Table</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DbColumnImpl extends ModelElementImpl implements DbColumn {
	/**
     * The default value of the '{@link #getDefault() <em>Default</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDefault()
     * @generated
     * @ordered
     */
	protected static final String DEFAULT_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getDefault() <em>Default</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getDefault()
     * @generated
     * @ordered
     */
	protected String default_ = DEFAULT_EDEFAULT;

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
     * The default value of the '{@link #getLength() <em>Length</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getLength()
     * @generated
     * @ordered
     */
	protected static final int LENGTH_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getLength() <em>Length</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getLength()
     * @generated
     * @ordered
     */
	protected int length = LENGTH_EDEFAULT;

	/**
     * The default value of the '{@link #getNullable() <em>Nullable</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getNullable()
     * @generated
     * @ordered
     */
	protected static final String NULLABLE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getNullable() <em>Nullable</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getNullable()
     * @generated
     * @ordered
     */
	protected String nullable = NULLABLE_EDEFAULT;

	/**
     * The default value of the '{@link #getScale() <em>Scale</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getScale()
     * @generated
     * @ordered
     */
	protected static final int SCALE_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getScale() <em>Scale</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getScale()
     * @generated
     * @ordered
     */
	protected int scale = SCALE_EDEFAULT;

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
     * The default value of the '{@link #getSize() <em>Size</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSize()
     * @generated
     * @ordered
     */
	protected static final int SIZE_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getSize() <em>Size</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSize()
     * @generated
     * @ordered
     */
	protected int size = SIZE_EDEFAULT;

	/**
     * The default value of the '{@link #getType() <em>Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getType()
     * @generated
     * @ordered
     */
	protected static final DbColumnTypeEnum TYPE_EDEFAULT = DbColumnTypeEnum.BIGINT;

	/**
     * The cached value of the '{@link #getType() <em>Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getType()
     * @generated
     * @ordered
     */
	protected DbColumnTypeEnum type = TYPE_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DbColumnImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DB_COLUMN;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getDefault() {
        return default_;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setDefault(String newDefault) {
        String oldDefault = default_;
        default_ = newDefault;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__DEFAULT, oldDefault, default_));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__KEY_SEQ, oldKeySeq, keySeq));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getLength() {
        return length;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setLength(int newLength) {
        int oldLength = length;
        length = newLength;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__LENGTH, oldLength, length));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getNullable() {
        return nullable;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setNullable(String newNullable) {
        String oldNullable = nullable;
        nullable = newNullable;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__NULLABLE, oldNullable, nullable));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getScale() {
        return scale;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setScale(int newScale) {
        int oldScale = scale;
        scale = newScale;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__SCALE, oldScale, scale));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__SEQ, oldSeq, seq));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getSize() {
        return size;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setSize(int newSize) {
        int oldSize = size;
        size = newSize;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__SIZE, oldSize, size));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbColumnTypeEnum getType() {
        return type;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setType(DbColumnTypeEnum newType) {
        DbColumnTypeEnum oldType = type;
        type = newType == null ? TYPE_EDEFAULT : newType;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__TYPE, oldType, type));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbTable getTable() {
        if (eContainerFeatureID() != Model2Package.DB_COLUMN__TABLE) return null;
        return (DbTable)eInternalContainer();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public NotificationChain basicSetTable(DbTable newTable, NotificationChain msgs) {
        msgs = eBasicSetContainer((InternalEObject)newTable, Model2Package.DB_COLUMN__TABLE, msgs);
        return msgs;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setTable(DbTable newTable) {
        if (newTable != eInternalContainer() || (eContainerFeatureID() != Model2Package.DB_COLUMN__TABLE && newTable != null)) {
            if (EcoreUtil.isAncestor(this, newTable))
                throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
            NotificationChain msgs = null;
            if (eInternalContainer() != null)
                msgs = eBasicRemoveFromContainer(msgs);
            if (newTable != null)
                msgs = ((InternalEObject)newTable).eInverseAdd(this, Model2Package.DB_TABLE__COLUMNS, DbTable.class, msgs);
            msgs = basicSetTable(newTable, msgs);
            if (msgs != null) msgs.dispatch();
        }
        else if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DB_COLUMN__TABLE, newTable, newTable));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.DB_COLUMN__TABLE:
                if (eInternalContainer() != null)
                    msgs = eBasicRemoveFromContainer(msgs);
                return basicSetTable((DbTable)otherEnd, msgs);
        }
        return super.eInverseAdd(otherEnd, featureID, msgs);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.DB_COLUMN__TABLE:
                return basicSetTable(null, msgs);
        }
        return super.eInverseRemove(otherEnd, featureID, msgs);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
        switch (eContainerFeatureID()) {
            case Model2Package.DB_COLUMN__TABLE:
                return eInternalContainer().eInverseRemove(this, Model2Package.DB_TABLE__COLUMNS, DbTable.class, msgs);
        }
        return super.eBasicRemoveFromContainerFeature(msgs);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.DB_COLUMN__DEFAULT:
                return getDefault();
            case Model2Package.DB_COLUMN__KEY_SEQ:
                return getKeySeq();
            case Model2Package.DB_COLUMN__LENGTH:
                return getLength();
            case Model2Package.DB_COLUMN__NULLABLE:
                return getNullable();
            case Model2Package.DB_COLUMN__SCALE:
                return getScale();
            case Model2Package.DB_COLUMN__SEQ:
                return getSeq();
            case Model2Package.DB_COLUMN__SIZE:
                return getSize();
            case Model2Package.DB_COLUMN__TYPE:
                return getType();
            case Model2Package.DB_COLUMN__TABLE:
                return getTable();
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
            case Model2Package.DB_COLUMN__DEFAULT:
                setDefault((String)newValue);
                return;
            case Model2Package.DB_COLUMN__KEY_SEQ:
                setKeySeq((Integer)newValue);
                return;
            case Model2Package.DB_COLUMN__LENGTH:
                setLength((Integer)newValue);
                return;
            case Model2Package.DB_COLUMN__NULLABLE:
                setNullable((String)newValue);
                return;
            case Model2Package.DB_COLUMN__SCALE:
                setScale((Integer)newValue);
                return;
            case Model2Package.DB_COLUMN__SEQ:
                setSeq((Integer)newValue);
                return;
            case Model2Package.DB_COLUMN__SIZE:
                setSize((Integer)newValue);
                return;
            case Model2Package.DB_COLUMN__TYPE:
                setType((DbColumnTypeEnum)newValue);
                return;
            case Model2Package.DB_COLUMN__TABLE:
                setTable((DbTable)newValue);
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
            case Model2Package.DB_COLUMN__DEFAULT:
                setDefault(DEFAULT_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__KEY_SEQ:
                setKeySeq(KEY_SEQ_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__LENGTH:
                setLength(LENGTH_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__NULLABLE:
                setNullable(NULLABLE_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__SCALE:
                setScale(SCALE_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__SEQ:
                setSeq(SEQ_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__SIZE:
                setSize(SIZE_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__TYPE:
                setType(TYPE_EDEFAULT);
                return;
            case Model2Package.DB_COLUMN__TABLE:
                setTable((DbTable)null);
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
            case Model2Package.DB_COLUMN__DEFAULT:
                return DEFAULT_EDEFAULT == null ? default_ != null : !DEFAULT_EDEFAULT.equals(default_);
            case Model2Package.DB_COLUMN__KEY_SEQ:
                return keySeq != KEY_SEQ_EDEFAULT;
            case Model2Package.DB_COLUMN__LENGTH:
                return length != LENGTH_EDEFAULT;
            case Model2Package.DB_COLUMN__NULLABLE:
                return NULLABLE_EDEFAULT == null ? nullable != null : !NULLABLE_EDEFAULT.equals(nullable);
            case Model2Package.DB_COLUMN__SCALE:
                return scale != SCALE_EDEFAULT;
            case Model2Package.DB_COLUMN__SEQ:
                return seq != SEQ_EDEFAULT;
            case Model2Package.DB_COLUMN__SIZE:
                return size != SIZE_EDEFAULT;
            case Model2Package.DB_COLUMN__TYPE:
                return type != TYPE_EDEFAULT;
            case Model2Package.DB_COLUMN__TABLE:
                return getTable() != null;
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
        result.append(" (default: ");
        result.append(default_);
        result.append(", keySeq: ");
        result.append(keySeq);
        result.append(", length: ");
        result.append(length);
        result.append(", nullable: ");
        result.append(nullable);
        result.append(", scale: ");
        result.append(scale);
        result.append(", seq: ");
        result.append(seq);
        result.append(", size: ");
        result.append(size);
        result.append(", type: ");
        result.append(type);
        result.append(')');
        return result.toString();
    }

} //DbColumnImpl
