/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Field</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Field#isMandatory <em>Mandatory</em>}</li>
 *   <li>{@link fw2.model2.Field#getMaxLength <em>Max Length</em>}</li>
 *   <li>{@link fw2.model2.Field#getReferenceListName <em>Reference List Name</em>}</li>
 *   <li>{@link fw2.model2.Field#getRowIndex <em>Row Index</em>}</li>
 *   <li>{@link fw2.model2.Field#getSelectDisabled <em>Select Disabled</em>}</li>
 *   <li>{@link fw2.model2.Field#getSelectHeaderValue <em>Select Header Value</em>}</li>
 *   <li>{@link fw2.model2.Field#getSize <em>Size</em>}</li>
 *   <li>{@link fw2.model2.Field#getPresentInStaging <em>Present In Staging</em>}</li>
 *   <li>{@link fw2.model2.Field#getComments <em>Comments</em>}</li>
 *   <li>{@link fw2.model2.Field#getMapping <em>Mapping</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getField()
 * @model extendedMetaData="name='field' kind='elementOnly'"
 * @generated
 */
public interface Field extends ViewPrimitive {
	/**
     * Returns the value of the '<em><b>Mandatory</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Mandatory</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Mandatory</em>' attribute.
     * @see #setMandatory(boolean)
     * @see fw2.model2.Model2Package#getField_Mandatory()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Boolean"
     *        extendedMetaData="kind='attribute' name='mandatory' namespace='##targetNamespace'"
     * @generated
     */
	boolean isMandatory();

	/**
     * Sets the value of the '{@link fw2.model2.Field#isMandatory <em>Mandatory</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Mandatory</em>' attribute.
     * @see #isMandatory()
     * @generated
     */
	void setMandatory(boolean value);

	/**
     * Returns the value of the '<em><b>Max Length</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Max Length</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Max Length</em>' attribute.
     * @see #setMaxLength(int)
     * @see fw2.model2.Model2Package#getField_MaxLength()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='maxLength' namespace='##targetNamespace'"
     * @generated
     */
	int getMaxLength();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getMaxLength <em>Max Length</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Max Length</em>' attribute.
     * @see #getMaxLength()
     * @generated
     */
	void setMaxLength(int value);

	/**
     * Returns the value of the '<em><b>Reference List Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reference List Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Reference List Name</em>' attribute.
     * @see #setReferenceListName(String)
     * @see fw2.model2.Model2Package#getField_ReferenceListName()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='attribute' name='referenceListName' namespace='##targetNamespace'"
     * @generated
     */
	String getReferenceListName();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getReferenceListName <em>Reference List Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Reference List Name</em>' attribute.
     * @see #getReferenceListName()
     * @generated
     */
	void setReferenceListName(String value);

	/**
     * Returns the value of the '<em><b>Row Index</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Row Index</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Row Index</em>' attribute.
     * @see #setRowIndex(int)
     * @see fw2.model2.Model2Package#getField_RowIndex()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='rowIndex' namespace='##targetNamespace'"
     * @generated
     */
	int getRowIndex();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getRowIndex <em>Row Index</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Row Index</em>' attribute.
     * @see #getRowIndex()
     * @generated
     */
	void setRowIndex(int value);

	/**
     * Returns the value of the '<em><b>Select Disabled</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Select Disabled</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Select Disabled</em>' attribute.
     * @see #setSelectDisabled(String)
     * @see fw2.model2.Model2Package#getField_SelectDisabled()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='attribute' name='selectDisabled' namespace='##targetNamespace'"
     * @generated
     */
	String getSelectDisabled();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getSelectDisabled <em>Select Disabled</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Select Disabled</em>' attribute.
     * @see #getSelectDisabled()
     * @generated
     */
	void setSelectDisabled(String value);

	/**
     * Returns the value of the '<em><b>Select Header Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Select Header Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Select Header Value</em>' attribute.
     * @see #setSelectHeaderValue(String)
     * @see fw2.model2.Model2Package#getField_SelectHeaderValue()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='attribute' name='selectHeaderValue' namespace='##targetNamespace'"
     * @generated
     */
	String getSelectHeaderValue();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getSelectHeaderValue <em>Select Header Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Select Header Value</em>' attribute.
     * @see #getSelectHeaderValue()
     * @generated
     */
	void setSelectHeaderValue(String value);

	/**
     * Returns the value of the '<em><b>Size</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Size</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Size</em>' attribute.
     * @see #setSize(int)
     * @see fw2.model2.Model2Package#getField_Size()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='size' namespace='##targetNamespace'"
     * @generated
     */
	int getSize();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getSize <em>Size</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Size</em>' attribute.
     * @see #getSize()
     * @generated
     */
	void setSize(int value);

	/**
     * Returns the value of the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Present In Staging</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Present In Staging</em>' attribute.
     * @see #setPresentInStaging(String)
     * @see fw2.model2.Model2Package#getField_PresentInStaging()
     * @model
     * @generated
     */
	String getPresentInStaging();

	/**
     * Sets the value of the '{@link fw2.model2.Field#getPresentInStaging <em>Present In Staging</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Present In Staging</em>' attribute.
     * @see #getPresentInStaging()
     * @generated
     */
	void setPresentInStaging(String value);

    /**
     * Returns the value of the '<em><b>Comments</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <p>
     * If the meaning of the '<em>Comments</em>' attribute isn't clear,
     * there really should be more of a description here...
     * </p>
     * <!-- end-user-doc -->
     * @return the value of the '<em>Comments</em>' attribute.
     * @see #setComments(String)
     * @see fw2.model2.Model2Package#getField_Comments()
     * @model
     * @generated
     */
    String getComments();

    /**
     * Sets the value of the '{@link fw2.model2.Field#getComments <em>Comments</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Comments</em>' attribute.
     * @see #getComments()
     * @generated
     */
    void setComments(String value);

    /**
     * Returns the value of the '<em><b>Mapping</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <p>
     * If the meaning of the '<em>Mapping</em>' attribute isn't clear,
     * there really should be more of a description here...
     * </p>
     * <!-- end-user-doc -->
     * @return the value of the '<em>Mapping</em>' attribute.
     * @see #setMapping(String)
     * @see fw2.model2.Model2Package#getField_Mapping()
     * @model
     * @generated
     */
    String getMapping();

    /**
     * Sets the value of the '{@link fw2.model2.Field#getMapping <em>Mapping</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Mapping</em>' attribute.
     * @see #getMapping()
     * @generated
     */
    void setMapping(String value);

} // Field
