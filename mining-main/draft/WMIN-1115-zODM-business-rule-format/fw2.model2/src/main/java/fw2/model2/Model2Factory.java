/**
 */
package fw2.model2;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see fw2.model2.Model2Package
 * @generated
 */
public interface Model2Factory extends EFactory {
	/**
     * The singleton instance of the factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	Model2Factory eINSTANCE = fw2.model2.impl.Model2FactoryImpl.init();

	/**
     * Returns a new object of class '<em>Action</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Action</em>'.
     * @generated
     */
	Action createAction();

	/**
     * Returns a new object of class '<em>Aggregation</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Aggregation</em>'.
     * @generated
     */
	Aggregation createAggregation();

	/**
     * Returns a new object of class '<em>Annotation</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Annotation</em>'.
     * @generated
     */
	Annotation createAnnotation();

	/**
     * Returns a new object of class '<em>Application</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Application</em>'.
     * @generated
     */
	Application createApplication();

	/**
     * Returns a new object of class '<em>Bap</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Bap</em>'.
     * @generated
     */
	Bap createBap();

	/**
     * Returns a new object of class '<em>Class Orm</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Class Orm</em>'.
     * @generated
     */
	ClassOrm createClassOrm();

	/**
     * Returns a new object of class '<em>Component</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Component</em>'.
     * @generated
     */
	Component createComponent();

	/**
     * Returns a new object of class '<em>Constant</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Constant</em>'.
     * @generated
     */
	Constant createConstant();

	/**
     * Returns a new object of class '<em>Db Column</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Column</em>'.
     * @generated
     */
	DbColumn createDbColumn();

	/**
     * Returns a new object of class '<em>Db Column Map</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Column Map</em>'.
     * @generated
     */
	DbColumnMap createDbColumnMap();

	/**
     * Returns a new object of class '<em>Db Foreign Key Column</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Foreign Key Column</em>'.
     * @generated
     */
	DbForeignKeyColumn createDbForeignKeyColumn();

	/**
     * Returns a new object of class '<em>Db Join Key</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Join Key</em>'.
     * @generated
     */
	DbJoinKey createDbJoinKey();

	/**
     * Returns a new object of class '<em>Db Join Parameter</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Join Parameter</em>'.
     * @generated
     */
	DbJoinParameter createDbJoinParameter();

	/**
     * Returns a new object of class '<em>Db Relation</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Relation</em>'.
     * @generated
     */
	DbRelation createDbRelation();

	/**
     * Returns a new object of class '<em>Db Table</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db Table</em>'.
     * @generated
     */
	DbTable createDbTable();

	/**
     * Returns a new object of class '<em>Db View</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db View</em>'.
     * @generated
     */
	DbView createDbView();

	/**
     * Returns a new object of class '<em>Db View Column</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Db View Column</em>'.
     * @generated
     */
	DbViewColumn createDbViewColumn();

	/**
     * Returns a new object of class '<em>Discriminator Key</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Discriminator Key</em>'.
     * @generated
     */
	DiscriminatorKey createDiscriminatorKey();

	/**
     * Returns a new object of class '<em>Domain Attribute</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Domain Attribute</em>'.
     * @generated
     */
	DomainAttribute createDomainAttribute();

	/**
     * Returns a new object of class '<em>Domain Class</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Domain Class</em>'.
     * @generated
     */
	DomainClass createDomainClass();

	/**
     * Returns a new object of class '<em>Editor</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Editor</em>'.
     * @generated
     */
	Editor createEditor();

	/**
     * Returns a new object of class '<em>Field</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Field</em>'.
     * @generated
     */
	Field createField();

	/**
     * Returns a new object of class '<em>Field Extension</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Field Extension</em>'.
     * @generated
     */
	FieldExtension createFieldExtension();

	/**
     * Returns a new object of class '<em>Field Group</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Field Group</em>'.
     * @generated
     */
	FieldGroup createFieldGroup();

	/**
     * Returns a new object of class '<em>Field Group Extension</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Field Group Extension</em>'.
     * @generated
     */
	FieldGroupExtension createFieldGroupExtension();

	/**
     * Returns a new object of class '<em>Lookup</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Lookup</em>'.
     * @generated
     */
	Lookup createLookup();

	/**
     * Returns a new object of class '<em>Model</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Model</em>'.
     * @generated
     */
	Model createModel();

	/**
     * Returns a new object of class '<em>Primitive</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Primitive</em>'.
     * @generated
     */
	Primitive createPrimitive();

	/**
     * Returns a new object of class '<em>Primitive Extension</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Primitive Extension</em>'.
     * @generated
     */
	PrimitiveExtension createPrimitiveExtension();

	/**
     * Returns a new object of class '<em>Property</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Property</em>'.
     * @generated
     */
	Property createProperty();

	/**
     * Returns a new object of class '<em>Reference</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Reference</em>'.
     * @generated
     */
	Reference createReference();

	/**
     * Returns a new object of class '<em>Summary Table</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Summary Table</em>'.
     * @generated
     */
	SummaryTable createSummaryTable();

	/**
     * Returns a new object of class '<em>Table Column</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Table Column</em>'.
     * @generated
     */
	TableColumn createTableColumn();

	/**
     * Returns a new object of class '<em>Tag</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Tag</em>'.
     * @generated
     */
	Tag createTag();

	/**
     * Returns a new object of class '<em>Type Extension</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Type Extension</em>'.
     * @generated
     */
	TypeExtension createTypeExtension();

	/**
     * Returns a new object of class '<em>Ui Field Type</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Ui Field Type</em>'.
     * @generated
     */
	UiFieldType createUiFieldType();

	/**
     * Returns a new object of class '<em>Ui Field Type Property</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Ui Field Type Property</em>'.
     * @generated
     */
	UiFieldTypeProperty createUiFieldTypeProperty();

	/**
     * Returns a new object of class '<em>Component Mapping</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Component Mapping</em>'.
     * @generated
     */
	ComponentMapping createComponentMapping();

	/**
     * Returns the package supported by this factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the package supported by this factory.
     * @generated
     */
	Model2Package getModel2Package();

} //Model2Factory
