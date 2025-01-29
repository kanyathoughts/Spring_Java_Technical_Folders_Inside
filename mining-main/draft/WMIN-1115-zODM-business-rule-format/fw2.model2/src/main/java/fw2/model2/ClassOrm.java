/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Class Orm</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.ClassOrm#getAggregations <em>Aggregations</em>}</li>
 *   <li>{@link fw2.model2.ClassOrm#getDataSet <em>Data Set</em>}</li>
 *   <li>{@link fw2.model2.ClassOrm#getKey <em>Key</em>}</li>
 *   <li>{@link fw2.model2.ClassOrm#getReferences <em>References</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getClassOrm()
 * @model extendedMetaData="name='classOrm' kind='elementOnly'"
 * @generated
 */
public interface ClassOrm extends Component {
	/**
     * Returns the value of the '<em><b>Aggregations</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Aggregation}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Aggregations</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Aggregations</em>' containment reference list.
     * @see fw2.model2.Model2Package#getClassOrm_Aggregations()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='aggregations' namespace='##targetNamespace'"
     * @generated
     */
	EList<Aggregation> getAggregations();

	/**
     * Returns the value of the '<em><b>Data Set</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Set</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Data Set</em>' reference.
     * @see #setDataSet(DbDataSet)
     * @see fw2.model2.Model2Package#getClassOrm_DataSet()
     * @model resolveProxies="false" required="true"
     *        extendedMetaData="kind='element' name='dataSet' namespace='##targetNamespace'"
     * @generated
     */
	DbDataSet getDataSet();

	/**
     * Sets the value of the '{@link fw2.model2.ClassOrm#getDataSet <em>Data Set</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Data Set</em>' reference.
     * @see #getDataSet()
     * @generated
     */
	void setDataSet(DbDataSet value);

	/**
     * Returns the value of the '<em><b>Key</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Key</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Key</em>' containment reference.
     * @see #setKey(DiscriminatorKey)
     * @see fw2.model2.Model2Package#getClassOrm_Key()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='key' namespace='##targetNamespace'"
     * @generated
     */
	DiscriminatorKey getKey();

	/**
     * Sets the value of the '{@link fw2.model2.ClassOrm#getKey <em>Key</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Key</em>' containment reference.
     * @see #getKey()
     * @generated
     */
	void setKey(DiscriminatorKey value);

	/**
     * Returns the value of the '<em><b>References</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Reference}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>References</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>References</em>' containment reference list.
     * @see fw2.model2.Model2Package#getClassOrm_References()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='references' namespace='##targetNamespace'"
     * @generated
     */
	EList<Reference> getReferences();

} // ClassOrm
