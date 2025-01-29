/**
 */
package fw2.orm.xlsx;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Mapping</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.Mapping#getClassMaps <em>Class Maps</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getMapping()
 * @model
 * @generated
 */
public interface Mapping extends MapElement {
	/**
	 * Returns the value of the '<em><b>Class Maps</b></em>' containment reference list.
	 * The list contents are of type {@link fw2.orm.xlsx.ClassMap}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Class Maps</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Class Maps</em>' containment reference list.
	 * @see fw2.orm.xlsx.XlsxPackage#getMapping_ClassMaps()
	 * @model containment="true"
	 * @generated
	 */
	EList<ClassMap> getClassMaps();

} // Mapping
