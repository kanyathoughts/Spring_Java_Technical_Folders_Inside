/**
 */
package fw2.model2.impl;

import fw2.model2.Field;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Field</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.FieldImpl#isMandatory <em>Mandatory</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getMaxLength <em>Max Length</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getReferenceListName <em>Reference List Name</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getRowIndex <em>Row Index</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getSelectDisabled <em>Select Disabled</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getSelectHeaderValue <em>Select Header Value</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getSize <em>Size</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getPresentInStaging <em>Present In Staging</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getComments <em>Comments</em>}</li>
 *   <li>{@link fw2.model2.impl.FieldImpl#getMapping <em>Mapping</em>}</li>
 * </ul>
 *
 * @generated
 */
public class FieldImpl extends ViewPrimitiveImpl implements Field {
	/**
     * The default value of the '{@link #isMandatory() <em>Mandatory</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isMandatory()
     * @generated
     * @ordered
     */
	protected static final boolean MANDATORY_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isMandatory() <em>Mandatory</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isMandatory()
     * @generated
     * @ordered
     */
	protected boolean mandatory = MANDATORY_EDEFAULT;

	/**
     * The default value of the '{@link #getMaxLength() <em>Max Length</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getMaxLength()
     * @generated
     * @ordered
     */
	protected static final int MAX_LENGTH_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getMaxLength() <em>Max Length</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getMaxLength()
     * @generated
     * @ordered
     */
	protected int maxLength = MAX_LENGTH_EDEFAULT;

	/**
     * The default value of the '{@link #getReferenceListName() <em>Reference List Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getReferenceListName()
     * @generated
     * @ordered
     */
	protected static final String REFERENCE_LIST_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getReferenceListName() <em>Reference List Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getReferenceListName()
     * @generated
     * @ordered
     */
	protected String referenceListName = REFERENCE_LIST_NAME_EDEFAULT;

	/**
     * The default value of the '{@link #getRowIndex() <em>Row Index</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRowIndex()
     * @generated
     * @ordered
     */
	protected static final int ROW_INDEX_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getRowIndex() <em>Row Index</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRowIndex()
     * @generated
     * @ordered
     */
	protected int rowIndex = ROW_INDEX_EDEFAULT;

	/**
     * The default value of the '{@link #getSelectDisabled() <em>Select Disabled</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSelectDisabled()
     * @generated
     * @ordered
     */
	protected static final String SELECT_DISABLED_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getSelectDisabled() <em>Select Disabled</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSelectDisabled()
     * @generated
     * @ordered
     */
	protected String selectDisabled = SELECT_DISABLED_EDEFAULT;

	/**
     * The default value of the '{@link #getSelectHeaderValue() <em>Select Header Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSelectHeaderValue()
     * @generated
     * @ordered
     */
	protected static final String SELECT_HEADER_VALUE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getSelectHeaderValue() <em>Select Header Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSelectHeaderValue()
     * @generated
     * @ordered
     */
	protected String selectHeaderValue = SELECT_HEADER_VALUE_EDEFAULT;

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
     * The default value of the '{@link #getComments() <em>Comments</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getComments()
     * @generated
     * @ordered
     */
    protected static final String COMMENTS_EDEFAULT = null;

    /**
     * The cached value of the '{@link #getComments() <em>Comments</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getComments()
     * @generated
     * @ordered
     */
    protected String comments = COMMENTS_EDEFAULT;

    /**
     * The default value of the '{@link #getMapping() <em>Mapping</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getMapping()
     * @generated
     * @ordered
     */
    protected static final String MAPPING_EDEFAULT = null;

    /**
     * The cached value of the '{@link #getMapping() <em>Mapping</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getMapping()
     * @generated
     * @ordered
     */
    protected String mapping = MAPPING_EDEFAULT;

    /**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected FieldImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.FIELD;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isMandatory() {
        return mandatory;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setMandatory(boolean newMandatory) {
        boolean oldMandatory = mandatory;
        mandatory = newMandatory;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__MANDATORY, oldMandatory, mandatory));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getMaxLength() {
        return maxLength;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setMaxLength(int newMaxLength) {
        int oldMaxLength = maxLength;
        maxLength = newMaxLength;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__MAX_LENGTH, oldMaxLength, maxLength));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getReferenceListName() {
        return referenceListName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setReferenceListName(String newReferenceListName) {
        String oldReferenceListName = referenceListName;
        referenceListName = newReferenceListName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__REFERENCE_LIST_NAME, oldReferenceListName, referenceListName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getRowIndex() {
        return rowIndex;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setRowIndex(int newRowIndex) {
        int oldRowIndex = rowIndex;
        rowIndex = newRowIndex;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__ROW_INDEX, oldRowIndex, rowIndex));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getSelectDisabled() {
        return selectDisabled;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setSelectDisabled(String newSelectDisabled) {
        String oldSelectDisabled = selectDisabled;
        selectDisabled = newSelectDisabled;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__SELECT_DISABLED, oldSelectDisabled, selectDisabled));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getSelectHeaderValue() {
        return selectHeaderValue;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setSelectHeaderValue(String newSelectHeaderValue) {
        String oldSelectHeaderValue = selectHeaderValue;
        selectHeaderValue = newSelectHeaderValue;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__SELECT_HEADER_VALUE, oldSelectHeaderValue, selectHeaderValue));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__SIZE, oldSize, size));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__PRESENT_IN_STAGING, oldPresentInStaging, presentInStaging));
    }

	/**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public String getComments() {
        return comments;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setComments(String newComments) {
        String oldComments = comments;
        comments = newComments;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__COMMENTS, oldComments, comments));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public String getMapping() {
        return mapping;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setMapping(String newMapping) {
        String oldMapping = mapping;
        mapping = newMapping;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.FIELD__MAPPING, oldMapping, mapping));
    }

    /**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.FIELD__MANDATORY:
                return isMandatory();
            case Model2Package.FIELD__MAX_LENGTH:
                return getMaxLength();
            case Model2Package.FIELD__REFERENCE_LIST_NAME:
                return getReferenceListName();
            case Model2Package.FIELD__ROW_INDEX:
                return getRowIndex();
            case Model2Package.FIELD__SELECT_DISABLED:
                return getSelectDisabled();
            case Model2Package.FIELD__SELECT_HEADER_VALUE:
                return getSelectHeaderValue();
            case Model2Package.FIELD__SIZE:
                return getSize();
            case Model2Package.FIELD__PRESENT_IN_STAGING:
                return getPresentInStaging();
            case Model2Package.FIELD__COMMENTS:
                return getComments();
            case Model2Package.FIELD__MAPPING:
                return getMapping();
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
            case Model2Package.FIELD__MANDATORY:
                setMandatory((Boolean)newValue);
                return;
            case Model2Package.FIELD__MAX_LENGTH:
                setMaxLength((Integer)newValue);
                return;
            case Model2Package.FIELD__REFERENCE_LIST_NAME:
                setReferenceListName((String)newValue);
                return;
            case Model2Package.FIELD__ROW_INDEX:
                setRowIndex((Integer)newValue);
                return;
            case Model2Package.FIELD__SELECT_DISABLED:
                setSelectDisabled((String)newValue);
                return;
            case Model2Package.FIELD__SELECT_HEADER_VALUE:
                setSelectHeaderValue((String)newValue);
                return;
            case Model2Package.FIELD__SIZE:
                setSize((Integer)newValue);
                return;
            case Model2Package.FIELD__PRESENT_IN_STAGING:
                setPresentInStaging((String)newValue);
                return;
            case Model2Package.FIELD__COMMENTS:
                setComments((String)newValue);
                return;
            case Model2Package.FIELD__MAPPING:
                setMapping((String)newValue);
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
            case Model2Package.FIELD__MANDATORY:
                setMandatory(MANDATORY_EDEFAULT);
                return;
            case Model2Package.FIELD__MAX_LENGTH:
                setMaxLength(MAX_LENGTH_EDEFAULT);
                return;
            case Model2Package.FIELD__REFERENCE_LIST_NAME:
                setReferenceListName(REFERENCE_LIST_NAME_EDEFAULT);
                return;
            case Model2Package.FIELD__ROW_INDEX:
                setRowIndex(ROW_INDEX_EDEFAULT);
                return;
            case Model2Package.FIELD__SELECT_DISABLED:
                setSelectDisabled(SELECT_DISABLED_EDEFAULT);
                return;
            case Model2Package.FIELD__SELECT_HEADER_VALUE:
                setSelectHeaderValue(SELECT_HEADER_VALUE_EDEFAULT);
                return;
            case Model2Package.FIELD__SIZE:
                setSize(SIZE_EDEFAULT);
                return;
            case Model2Package.FIELD__PRESENT_IN_STAGING:
                setPresentInStaging(PRESENT_IN_STAGING_EDEFAULT);
                return;
            case Model2Package.FIELD__COMMENTS:
                setComments(COMMENTS_EDEFAULT);
                return;
            case Model2Package.FIELD__MAPPING:
                setMapping(MAPPING_EDEFAULT);
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
            case Model2Package.FIELD__MANDATORY:
                return mandatory != MANDATORY_EDEFAULT;
            case Model2Package.FIELD__MAX_LENGTH:
                return maxLength != MAX_LENGTH_EDEFAULT;
            case Model2Package.FIELD__REFERENCE_LIST_NAME:
                return REFERENCE_LIST_NAME_EDEFAULT == null ? referenceListName != null : !REFERENCE_LIST_NAME_EDEFAULT.equals(referenceListName);
            case Model2Package.FIELD__ROW_INDEX:
                return rowIndex != ROW_INDEX_EDEFAULT;
            case Model2Package.FIELD__SELECT_DISABLED:
                return SELECT_DISABLED_EDEFAULT == null ? selectDisabled != null : !SELECT_DISABLED_EDEFAULT.equals(selectDisabled);
            case Model2Package.FIELD__SELECT_HEADER_VALUE:
                return SELECT_HEADER_VALUE_EDEFAULT == null ? selectHeaderValue != null : !SELECT_HEADER_VALUE_EDEFAULT.equals(selectHeaderValue);
            case Model2Package.FIELD__SIZE:
                return size != SIZE_EDEFAULT;
            case Model2Package.FIELD__PRESENT_IN_STAGING:
                return PRESENT_IN_STAGING_EDEFAULT == null ? presentInStaging != null : !PRESENT_IN_STAGING_EDEFAULT.equals(presentInStaging);
            case Model2Package.FIELD__COMMENTS:
                return COMMENTS_EDEFAULT == null ? comments != null : !COMMENTS_EDEFAULT.equals(comments);
            case Model2Package.FIELD__MAPPING:
                return MAPPING_EDEFAULT == null ? mapping != null : !MAPPING_EDEFAULT.equals(mapping);
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
        result.append(" (mandatory: ");
        result.append(mandatory);
        result.append(", maxLength: ");
        result.append(maxLength);
        result.append(", referenceListName: ");
        result.append(referenceListName);
        result.append(", rowIndex: ");
        result.append(rowIndex);
        result.append(", selectDisabled: ");
        result.append(selectDisabled);
        result.append(", selectHeaderValue: ");
        result.append(selectHeaderValue);
        result.append(", size: ");
        result.append(size);
        result.append(", presentInStaging: ");
        result.append(presentInStaging);
        result.append(", comments: ");
        result.append(comments);
        result.append(", mapping: ");
        result.append(mapping);
        result.append(')');
        return result.toString();
    }

} //FieldImpl
