/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Table Column</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.TableColumn#getHeaderValue <em>Header Value</em>}</li>
 *   <li>{@link fw2.model2.TableColumn#getIndex <em>Index</em>}</li>
 *   <li>{@link fw2.model2.TableColumn#getProperty <em>Property</em>}</li>
 *   <li>{@link fw2.model2.TableColumn#getReferenceListName <em>Reference List Name</em>}</li>
 *   <li>{@link fw2.model2.TableColumn#getSelectDisabled <em>Select Disabled</em>}</li>
 *   <li>{@link fw2.model2.TableColumn#getComments <em>Comments</em>}</li>
 *   <li>{@link fw2.model2.TableColumn#getMapping <em>Mapping</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getTableColumn()
 * @model extendedMetaData="name='tableColumn' kind='elementOnly'"
 * @generated
 */
public interface TableColumn extends ViewPrimitive {
	/**
     * Returns the value of the '<em><b>Header Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Header Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Header Value</em>' attribute.
     * @see #setHeaderValue(String)
     * @see fw2.model2.Model2Package#getTableColumn_HeaderValue()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='attribute' name='headerValue' namespace='##targetNamespace'"
     * @generated
     */
	String getHeaderValue();

	/**
     * Sets the value of the '{@link fw2.model2.TableColumn#getHeaderValue <em>Header Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Header Value</em>' attribute.
     * @see #getHeaderValue()
     * @generated
     */
	void setHeaderValue(String value);

	/**
     * Returns the value of the '<em><b>Index</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Index</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Index</em>' attribute.
     * @see #setIndex(int)
     * @see fw2.model2.Model2Package#getTableColumn_Index()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='index' namespace='##targetNamespace'"
     * @generated
     */
	int getIndex();

	/**
     * Sets the value of the '{@link fw2.model2.TableColumn#getIndex <em>Index</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Index</em>' attribute.
     * @see #getIndex()
     * @generated
     */
	void setIndex(int value);

	/**
     * Returns the value of the '<em><b>Property</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Property</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Property</em>' attribute.
     * @see #setProperty(String)
     * @see fw2.model2.Model2Package#getTableColumn_Property()
     * @model
     * @generated
     */
	String getProperty();

	/**
     * Sets the value of the '{@link fw2.model2.TableColumn#getProperty <em>Property</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Property</em>' attribute.
     * @see #getProperty()
     * @generated
     */
	void setProperty(String value);

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
     * @see fw2.model2.Model2Package#getTableColumn_ReferenceListName()
     * @model
     * @generated
     */
	String getReferenceListName();

	/**
     * Sets the value of the '{@link fw2.model2.TableColumn#getReferenceListName <em>Reference List Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Reference List Name</em>' attribute.
     * @see #getReferenceListName()
     * @generated
     */
	void setReferenceListName(String value);

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
     * @see fw2.model2.Model2Package#getTableColumn_SelectDisabled()
     * @model
     * @generated
     */
	String getSelectDisabled();

	/**
     * Sets the value of the '{@link fw2.model2.TableColumn#getSelectDisabled <em>Select Disabled</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Select Disabled</em>' attribute.
     * @see #getSelectDisabled()
     * @generated
     */
	void setSelectDisabled(String value);

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
     * @see fw2.model2.Model2Package#getTableColumn_Comments()
     * @model
     * @generated
     */
    String getComments();

    /**
     * Sets the value of the '{@link fw2.model2.TableColumn#getComments <em>Comments</em>}' attribute.
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
     * @see fw2.model2.Model2Package#getTableColumn_Mapping()
     * @model
     * @generated
     */
    String getMapping();

    /**
     * Sets the value of the '{@link fw2.model2.TableColumn#getMapping <em>Mapping</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Mapping</em>' attribute.
     * @see #getMapping()
     * @generated
     */
    void setMapping(String value);

} // TableColumn
