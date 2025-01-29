/**
 */
package fw2.model2.impl;

import fw2.model2.Model2Package;
import fw2.model2.TableColumn;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Table Column</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getHeaderValue <em>Header Value</em>}</li>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getIndex <em>Index</em>}</li>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getProperty <em>Property</em>}</li>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getReferenceListName <em>Reference List Name</em>}</li>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getSelectDisabled <em>Select Disabled</em>}</li>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getComments <em>Comments</em>}</li>
 *   <li>{@link fw2.model2.impl.TableColumnImpl#getMapping <em>Mapping</em>}</li>
 * </ul>
 *
 * @generated
 */
public class TableColumnImpl extends ViewPrimitiveImpl implements TableColumn {
	/**
     * The default value of the '{@link #getHeaderValue() <em>Header Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHeaderValue()
     * @generated
     * @ordered
     */
	protected static final String HEADER_VALUE_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getHeaderValue() <em>Header Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHeaderValue()
     * @generated
     * @ordered
     */
	protected String headerValue = HEADER_VALUE_EDEFAULT;

	/**
     * The default value of the '{@link #getIndex() <em>Index</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getIndex()
     * @generated
     * @ordered
     */
	protected static final int INDEX_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getIndex() <em>Index</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getIndex()
     * @generated
     * @ordered
     */
	protected int index = INDEX_EDEFAULT;

	/**
     * The default value of the '{@link #getProperty() <em>Property</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getProperty()
     * @generated
     * @ordered
     */
	protected static final String PROPERTY_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getProperty() <em>Property</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getProperty()
     * @generated
     * @ordered
     */
	protected String property = PROPERTY_EDEFAULT;

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
	protected TableColumnImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.TABLE_COLUMN;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getHeaderValue() {
        return headerValue;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setHeaderValue(String newHeaderValue) {
        String oldHeaderValue = headerValue;
        headerValue = newHeaderValue;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__HEADER_VALUE, oldHeaderValue, headerValue));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getIndex() {
        return index;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setIndex(int newIndex) {
        int oldIndex = index;
        index = newIndex;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__INDEX, oldIndex, index));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getProperty() {
        return property;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setProperty(String newProperty) {
        String oldProperty = property;
        property = newProperty;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__PROPERTY, oldProperty, property));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__REFERENCE_LIST_NAME, oldReferenceListName, referenceListName));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__SELECT_DISABLED, oldSelectDisabled, selectDisabled));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__COMMENTS, oldComments, comments));
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
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.TABLE_COLUMN__MAPPING, oldMapping, mapping));
    }

    /**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.TABLE_COLUMN__HEADER_VALUE:
                return getHeaderValue();
            case Model2Package.TABLE_COLUMN__INDEX:
                return getIndex();
            case Model2Package.TABLE_COLUMN__PROPERTY:
                return getProperty();
            case Model2Package.TABLE_COLUMN__REFERENCE_LIST_NAME:
                return getReferenceListName();
            case Model2Package.TABLE_COLUMN__SELECT_DISABLED:
                return getSelectDisabled();
            case Model2Package.TABLE_COLUMN__COMMENTS:
                return getComments();
            case Model2Package.TABLE_COLUMN__MAPPING:
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
            case Model2Package.TABLE_COLUMN__HEADER_VALUE:
                setHeaderValue((String)newValue);
                return;
            case Model2Package.TABLE_COLUMN__INDEX:
                setIndex((Integer)newValue);
                return;
            case Model2Package.TABLE_COLUMN__PROPERTY:
                setProperty((String)newValue);
                return;
            case Model2Package.TABLE_COLUMN__REFERENCE_LIST_NAME:
                setReferenceListName((String)newValue);
                return;
            case Model2Package.TABLE_COLUMN__SELECT_DISABLED:
                setSelectDisabled((String)newValue);
                return;
            case Model2Package.TABLE_COLUMN__COMMENTS:
                setComments((String)newValue);
                return;
            case Model2Package.TABLE_COLUMN__MAPPING:
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
            case Model2Package.TABLE_COLUMN__HEADER_VALUE:
                setHeaderValue(HEADER_VALUE_EDEFAULT);
                return;
            case Model2Package.TABLE_COLUMN__INDEX:
                setIndex(INDEX_EDEFAULT);
                return;
            case Model2Package.TABLE_COLUMN__PROPERTY:
                setProperty(PROPERTY_EDEFAULT);
                return;
            case Model2Package.TABLE_COLUMN__REFERENCE_LIST_NAME:
                setReferenceListName(REFERENCE_LIST_NAME_EDEFAULT);
                return;
            case Model2Package.TABLE_COLUMN__SELECT_DISABLED:
                setSelectDisabled(SELECT_DISABLED_EDEFAULT);
                return;
            case Model2Package.TABLE_COLUMN__COMMENTS:
                setComments(COMMENTS_EDEFAULT);
                return;
            case Model2Package.TABLE_COLUMN__MAPPING:
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
            case Model2Package.TABLE_COLUMN__HEADER_VALUE:
                return HEADER_VALUE_EDEFAULT == null ? headerValue != null : !HEADER_VALUE_EDEFAULT.equals(headerValue);
            case Model2Package.TABLE_COLUMN__INDEX:
                return index != INDEX_EDEFAULT;
            case Model2Package.TABLE_COLUMN__PROPERTY:
                return PROPERTY_EDEFAULT == null ? property != null : !PROPERTY_EDEFAULT.equals(property);
            case Model2Package.TABLE_COLUMN__REFERENCE_LIST_NAME:
                return REFERENCE_LIST_NAME_EDEFAULT == null ? referenceListName != null : !REFERENCE_LIST_NAME_EDEFAULT.equals(referenceListName);
            case Model2Package.TABLE_COLUMN__SELECT_DISABLED:
                return SELECT_DISABLED_EDEFAULT == null ? selectDisabled != null : !SELECT_DISABLED_EDEFAULT.equals(selectDisabled);
            case Model2Package.TABLE_COLUMN__COMMENTS:
                return COMMENTS_EDEFAULT == null ? comments != null : !COMMENTS_EDEFAULT.equals(comments);
            case Model2Package.TABLE_COLUMN__MAPPING:
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
        result.append(" (headerValue: ");
        result.append(headerValue);
        result.append(", index: ");
        result.append(index);
        result.append(", property: ");
        result.append(property);
        result.append(", referenceListName: ");
        result.append(referenceListName);
        result.append(", selectDisabled: ");
        result.append(selectDisabled);
        result.append(", comments: ");
        result.append(comments);
        result.append(", mapping: ");
        result.append(mapping);
        result.append(')');
        return result.toString();
    }

} //TableColumnImpl
