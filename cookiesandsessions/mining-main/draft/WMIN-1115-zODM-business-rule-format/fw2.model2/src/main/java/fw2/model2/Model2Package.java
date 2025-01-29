/**
 */
package fw2.model2;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see fw2.model2.Model2Factory
 * @model kind="package"
 *        extendedMetaData="qualified='false'"
 * @generated
 */
public interface Model2Package extends EPackage {
	/**
     * The package name.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	String eNAME = "model2";

	/**
     * The package namespace URI.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	String eNS_URI = "http://fw2.model2/ecore/1.0";

	/**
     * The package namespace name.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	String eNS_PREFIX = "fw2.model2";

	/**
     * The singleton instance of the package.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	Model2Package eINSTANCE = fw2.model2.impl.Model2PackageImpl.init();

	/**
     * The meta object id for the '{@link fw2.model2.impl.ModelElementImpl <em>Model Element</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ModelElementImpl
     * @see fw2.model2.impl.Model2PackageImpl#getModelElement()
     * @generated
     */
	int MODEL_ELEMENT = 30;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL_ELEMENT__ANNOTATIONS = 0;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL_ELEMENT__NAME = 1;

	/**
     * The number of structural features of the '<em>Model Element</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL_ELEMENT_FEATURE_COUNT = 2;

	/**
     * The number of operations of the '<em>Model Element</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL_ELEMENT_OPERATION_COUNT = 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ViewElementImpl <em>View Element</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ViewElementImpl
     * @see fw2.model2.impl.Model2PackageImpl#getViewElement()
     * @generated
     */
	int VIEW_ELEMENT = 40;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT__ID = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT__LABEL = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT__TYPE = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of structural features of the '<em>View Element</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 3;

	/**
     * The number of operations of the '<em>View Element</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_ELEMENT_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ViewPrimitiveImpl <em>View Primitive</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ViewPrimitiveImpl
     * @see fw2.model2.impl.Model2PackageImpl#getViewPrimitive()
     * @generated
     */
	int VIEW_PRIMITIVE = 41;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE__ANNOTATIONS = VIEW_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE__NAME = VIEW_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE__ID = VIEW_ELEMENT__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE__LABEL = VIEW_ELEMENT__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE__TYPE = VIEW_ELEMENT__TYPE;

	/**
     * The number of structural features of the '<em>View Primitive</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE_FEATURE_COUNT = VIEW_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>View Primitive</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_PRIMITIVE_OPERATION_COUNT = VIEW_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ActionImpl <em>Action</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ActionImpl
     * @see fw2.model2.impl.Model2PackageImpl#getAction()
     * @generated
     */
	int ACTION = 0;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__ANNOTATIONS = VIEW_PRIMITIVE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__NAME = VIEW_PRIMITIVE__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__ID = VIEW_PRIMITIVE__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__LABEL = VIEW_PRIMITIVE__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__TYPE = VIEW_PRIMITIVE__TYPE;

	/**
     * The feature id for the '<em><b>Access Key</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__ACCESS_KEY = VIEW_PRIMITIVE_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Url</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION__URL = VIEW_PRIMITIVE_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Action</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION_FEATURE_COUNT = VIEW_PRIMITIVE_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Action</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ACTION_OPERATION_COUNT = VIEW_PRIMITIVE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.AttributeImpl <em>Attribute</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.AttributeImpl
     * @see fw2.model2.impl.Model2PackageImpl#getAttribute()
     * @generated
     */
	int ATTRIBUTE = 5;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ATTRIBUTE__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ATTRIBUTE__NAME = MODEL_ELEMENT__NAME;

	/**
     * The number of structural features of the '<em>Attribute</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ATTRIBUTE_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Attribute</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ATTRIBUTE_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.AssociationImpl <em>Association</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.AssociationImpl
     * @see fw2.model2.impl.Model2PackageImpl#getAssociation()
     * @generated
     */
	int ASSOCIATION = 4;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ASSOCIATION__ANNOTATIONS = ATTRIBUTE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ASSOCIATION__NAME = ATTRIBUTE__NAME;

	/**
     * The feature id for the '<em><b>Cardinality</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ASSOCIATION__CARDINALITY = ATTRIBUTE_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Related Class Orm</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ASSOCIATION__RELATED_CLASS_ORM = ATTRIBUTE_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Association</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ASSOCIATION_FEATURE_COUNT = ATTRIBUTE_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Association</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ASSOCIATION_OPERATION_COUNT = ATTRIBUTE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.AggregationImpl <em>Aggregation</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.AggregationImpl
     * @see fw2.model2.impl.Model2PackageImpl#getAggregation()
     * @generated
     */
	int AGGREGATION = 1;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION__ANNOTATIONS = ASSOCIATION__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION__NAME = ASSOCIATION__NAME;

	/**
     * The feature id for the '<em><b>Cardinality</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION__CARDINALITY = ASSOCIATION__CARDINALITY;

	/**
     * The feature id for the '<em><b>Related Class Orm</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION__RELATED_CLASS_ORM = ASSOCIATION__RELATED_CLASS_ORM;

	/**
     * The feature id for the '<em><b>Component</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION__COMPONENT = ASSOCIATION_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Aggregation</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION_FEATURE_COUNT = ASSOCIATION_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Aggregation</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int AGGREGATION_OPERATION_COUNT = ASSOCIATION_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.AnnotationImpl <em>Annotation</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.AnnotationImpl
     * @see fw2.model2.impl.Model2PackageImpl#getAnnotation()
     * @generated
     */
	int ANNOTATION = 2;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ANNOTATION__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ANNOTATION__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Tags</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ANNOTATION__TAGS = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Annotation</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ANNOTATION_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Annotation</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int ANNOTATION_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ViewComponentImpl <em>View Component</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ViewComponentImpl
     * @see fw2.model2.impl.Model2PackageImpl#getViewComponent()
     * @generated
     */
	int VIEW_COMPONENT = 39;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT__ANNOTATIONS = VIEW_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT__NAME = VIEW_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT__ID = VIEW_ELEMENT__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT__LABEL = VIEW_ELEMENT__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT__TYPE = VIEW_ELEMENT__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT__COMPONENT_ID = VIEW_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>View Component</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT_FEATURE_COUNT = VIEW_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>View Component</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int VIEW_COMPONENT_OPERATION_COUNT = VIEW_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ApplicationImpl <em>Application</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ApplicationImpl
     * @see fw2.model2.impl.Model2PackageImpl#getApplication()
     * @generated
     */
	int APPLICATION = 3;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__ANNOTATIONS = VIEW_COMPONENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__NAME = VIEW_COMPONENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__ID = VIEW_COMPONENT__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__LABEL = VIEW_COMPONENT__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__TYPE = VIEW_COMPONENT__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__COMPONENT_ID = VIEW_COMPONENT__COMPONENT_ID;

	/**
     * The feature id for the '<em><b>Baps</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION__BAPS = VIEW_COMPONENT_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Application</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION_FEATURE_COUNT = VIEW_COMPONENT_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Application</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int APPLICATION_OPERATION_COUNT = VIEW_COMPONENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.EditorImpl <em>Editor</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.EditorImpl
     * @see fw2.model2.impl.Model2PackageImpl#getEditor()
     * @generated
     */
	int EDITOR = 23;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__ANNOTATIONS = VIEW_COMPONENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__NAME = VIEW_COMPONENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__ID = VIEW_COMPONENT__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__LABEL = VIEW_COMPONENT__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__TYPE = VIEW_COMPONENT__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__COMPONENT_ID = VIEW_COMPONENT__COMPONENT_ID;

	/**
     * The feature id for the '<em><b>Editors</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__EDITORS = VIEW_COMPONENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Field Groups</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__FIELD_GROUPS = VIEW_COMPONENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Summary Tables</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__SUMMARY_TABLES = VIEW_COMPONENT_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR__PRESENT_IN_STAGING = VIEW_COMPONENT_FEATURE_COUNT + 3;

	/**
     * The number of structural features of the '<em>Editor</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR_FEATURE_COUNT = VIEW_COMPONENT_FEATURE_COUNT + 4;

	/**
     * The number of operations of the '<em>Editor</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int EDITOR_OPERATION_COUNT = VIEW_COMPONENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.BapImpl <em>Bap</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.BapImpl
     * @see fw2.model2.impl.Model2PackageImpl#getBap()
     * @generated
     */
	int BAP = 6;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__ANNOTATIONS = EDITOR__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__NAME = EDITOR__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__ID = EDITOR__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__LABEL = EDITOR__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__TYPE = EDITOR__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__COMPONENT_ID = EDITOR__COMPONENT_ID;

	/**
     * The feature id for the '<em><b>Editors</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__EDITORS = EDITOR__EDITORS;

	/**
     * The feature id for the '<em><b>Field Groups</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__FIELD_GROUPS = EDITOR__FIELD_GROUPS;

	/**
     * The feature id for the '<em><b>Summary Tables</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__SUMMARY_TABLES = EDITOR__SUMMARY_TABLES;

	/**
     * The feature id for the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__PRESENT_IN_STAGING = EDITOR__PRESENT_IN_STAGING;

	/**
     * The feature id for the '<em><b>Component Mapping</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP__COMPONENT_MAPPING = EDITOR_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Bap</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP_FEATURE_COUNT = EDITOR_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Bap</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int BAP_OPERATION_COUNT = EDITOR_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ComponentImpl <em>Component</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ComponentImpl
     * @see fw2.model2.impl.Model2PackageImpl#getComponent()
     * @generated
     */
	int COMPONENT = 8;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Lookups</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT__LOOKUPS = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Primitives</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT__PRIMITIVES = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Component</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Component</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ClassOrmImpl <em>Class Orm</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ClassOrmImpl
     * @see fw2.model2.impl.Model2PackageImpl#getClassOrm()
     * @generated
     */
	int CLASS_ORM = 7;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__ANNOTATIONS = COMPONENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__NAME = COMPONENT__NAME;

	/**
     * The feature id for the '<em><b>Lookups</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__LOOKUPS = COMPONENT__LOOKUPS;

	/**
     * The feature id for the '<em><b>Primitives</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__PRIMITIVES = COMPONENT__PRIMITIVES;

	/**
     * The feature id for the '<em><b>Aggregations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__AGGREGATIONS = COMPONENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Data Set</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__DATA_SET = COMPONENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Key</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__KEY = COMPONENT_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>References</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM__REFERENCES = COMPONENT_FEATURE_COUNT + 3;

	/**
     * The number of structural features of the '<em>Class Orm</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM_FEATURE_COUNT = COMPONENT_FEATURE_COUNT + 4;

	/**
     * The number of operations of the '<em>Class Orm</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CLASS_ORM_OPERATION_COUNT = COMPONENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.PrimitiveImpl <em>Primitive</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.PrimitiveImpl
     * @see fw2.model2.impl.Model2PackageImpl#getPrimitive()
     * @generated
     */
	int PRIMITIVE = 31;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE__ANNOTATIONS = ATTRIBUTE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE__NAME = ATTRIBUTE__NAME;

	/**
     * The number of structural features of the '<em>Primitive</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_FEATURE_COUNT = ATTRIBUTE_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Primitive</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_OPERATION_COUNT = ATTRIBUTE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ConstantImpl <em>Constant</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ConstantImpl
     * @see fw2.model2.impl.Model2PackageImpl#getConstant()
     * @generated
     */
	int CONSTANT = 9;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CONSTANT__ANNOTATIONS = PRIMITIVE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CONSTANT__NAME = PRIMITIVE__NAME;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CONSTANT__TYPE = PRIMITIVE_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CONSTANT__VALUE = PRIMITIVE_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Constant</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CONSTANT_FEATURE_COUNT = PRIMITIVE_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Constant</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int CONSTANT_OPERATION_COUNT = PRIMITIVE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbColumnImpl <em>Db Column</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbColumnImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbColumn()
     * @generated
     */
	int DB_COLUMN = 10;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Default</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__DEFAULT = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Key Seq</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__KEY_SEQ = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Length</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__LENGTH = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>Nullable</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__NULLABLE = MODEL_ELEMENT_FEATURE_COUNT + 3;

	/**
     * The feature id for the '<em><b>Scale</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__SCALE = MODEL_ELEMENT_FEATURE_COUNT + 4;

	/**
     * The feature id for the '<em><b>Seq</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__SEQ = MODEL_ELEMENT_FEATURE_COUNT + 5;

	/**
     * The feature id for the '<em><b>Size</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__SIZE = MODEL_ELEMENT_FEATURE_COUNT + 6;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__TYPE = MODEL_ELEMENT_FEATURE_COUNT + 7;

	/**
     * The feature id for the '<em><b>Table</b></em>' container reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN__TABLE = MODEL_ELEMENT_FEATURE_COUNT + 8;

	/**
     * The number of structural features of the '<em>Db Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 9;

	/**
     * The number of operations of the '<em>Db Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbColumnMapImpl <em>Db Column Map</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbColumnMapImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbColumnMap()
     * @generated
     */
	int DB_COLUMN_MAP = 11;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_MAP__ANNOTATIONS = PRIMITIVE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_MAP__NAME = PRIMITIVE__NAME;

	/**
     * The feature id for the '<em><b>Db Column Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_MAP__DB_COLUMN_NAME = PRIMITIVE_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Db Column Map</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_MAP_FEATURE_COUNT = PRIMITIVE_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Db Column Map</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_COLUMN_MAP_OPERATION_COUNT = PRIMITIVE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbDataSetImpl <em>Db Data Set</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbDataSetImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbDataSet()
     * @generated
     */
	int DB_DATA_SET = 12;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_DATA_SET__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_DATA_SET__NAME = MODEL_ELEMENT__NAME;

	/**
     * The number of structural features of the '<em>Db Data Set</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_DATA_SET_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Db Data Set</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_DATA_SET_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbForeignKeyColumnImpl <em>Db Foreign Key Column</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbForeignKeyColumnImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbForeignKeyColumn()
     * @generated
     */
	int DB_FOREIGN_KEY_COLUMN = 13;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_FOREIGN_KEY_COLUMN__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_FOREIGN_KEY_COLUMN__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Key Seq</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_FOREIGN_KEY_COLUMN__KEY_SEQ = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Seq</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_FOREIGN_KEY_COLUMN__SEQ = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Db Foreign Key Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_FOREIGN_KEY_COLUMN_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Db Foreign Key Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_FOREIGN_KEY_COLUMN_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbJoinKeyImpl <em>Db Join Key</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbJoinKeyImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbJoinKey()
     * @generated
     */
	int DB_JOIN_KEY = 14;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_KEY__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_KEY__NAME = MODEL_ELEMENT__NAME;

	/**
     * The number of structural features of the '<em>Db Join Key</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_KEY_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Db Join Key</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_KEY_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbJoinParameterImpl <em>Db Join Parameter</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbJoinParameterImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbJoinParameter()
     * @generated
     */
	int DB_JOIN_PARAMETER = 15;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_PARAMETER__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_PARAMETER__NAME = MODEL_ELEMENT__NAME;

	/**
     * The number of structural features of the '<em>Db Join Parameter</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_PARAMETER_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Db Join Parameter</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_JOIN_PARAMETER_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbRelationImpl <em>Db Relation</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbRelationImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbRelation()
     * @generated
     */
	int DB_RELATION = 16;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Foreign Key Columns</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION__FOREIGN_KEY_COLUMNS = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Ref Table</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION__REF_TABLE = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Table</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION__TABLE = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of structural features of the '<em>Db Relation</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 3;

	/**
     * The number of operations of the '<em>Db Relation</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_RELATION_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbTableImpl <em>Db Table</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbTableImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbTable()
     * @generated
     */
	int DB_TABLE = 17;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_TABLE__ANNOTATIONS = DB_DATA_SET__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_TABLE__NAME = DB_DATA_SET__NAME;

	/**
     * The feature id for the '<em><b>Columns</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_TABLE__COLUMNS = DB_DATA_SET_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Db Table</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_TABLE_FEATURE_COUNT = DB_DATA_SET_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Db Table</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_TABLE_OPERATION_COUNT = DB_DATA_SET_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbViewImpl <em>Db View</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbViewImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbView()
     * @generated
     */
	int DB_VIEW = 18;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW__ANNOTATIONS = DB_DATA_SET__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW__NAME = DB_DATA_SET__NAME;

	/**
     * The feature id for the '<em><b>Join Keys</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW__JOIN_KEYS = DB_DATA_SET_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Join Parameters</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW__JOIN_PARAMETERS = DB_DATA_SET_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>View Columns</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW__VIEW_COLUMNS = DB_DATA_SET_FEATURE_COUNT + 2;

	/**
     * The number of structural features of the '<em>Db View</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW_FEATURE_COUNT = DB_DATA_SET_FEATURE_COUNT + 3;

	/**
     * The number of operations of the '<em>Db View</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW_OPERATION_COUNT = DB_DATA_SET_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DbViewColumnImpl <em>Db View Column</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DbViewColumnImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDbViewColumn()
     * @generated
     */
	int DB_VIEW_COLUMN = 19;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW_COLUMN__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW_COLUMN__NAME = MODEL_ELEMENT__NAME;

	/**
     * The number of structural features of the '<em>Db View Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW_COLUMN_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Db View Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DB_VIEW_COLUMN_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DiscriminatorKeyImpl <em>Discriminator Key</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DiscriminatorKeyImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDiscriminatorKey()
     * @generated
     */
	int DISCRIMINATOR_KEY = 20;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DISCRIMINATOR_KEY__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DISCRIMINATOR_KEY__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Discriminator Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Discriminator Key</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DISCRIMINATOR_KEY_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Discriminator Key</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DISCRIMINATOR_KEY_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DomainAttributeImpl <em>Domain Attribute</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DomainAttributeImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDomainAttribute()
     * @generated
     */
	int DOMAIN_ATTRIBUTE = 21;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Data Type</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE__DATA_TYPE = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Multivalued</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE__MULTIVALUED = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Reference</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE__REFERENCE = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of structural features of the '<em>Domain Attribute</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 3;

	/**
     * The number of operations of the '<em>Domain Attribute</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_ATTRIBUTE_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.DomainClassImpl <em>Domain Class</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.DomainClassImpl
     * @see fw2.model2.impl.Model2PackageImpl#getDomainClass()
     * @generated
     */
	int DOMAIN_CLASS = 22;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Superclass</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__SUPERCLASS = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Platform Class</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__PLATFORM_CLASS = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Attributes</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__ATTRIBUTES = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>Abstract</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__ABSTRACT = MODEL_ELEMENT_FEATURE_COUNT + 3;

	/**
     * The feature id for the '<em><b>Primitive</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS__PRIMITIVE = MODEL_ELEMENT_FEATURE_COUNT + 4;

	/**
     * The number of structural features of the '<em>Domain Class</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 5;

	/**
     * The number of operations of the '<em>Domain Class</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int DOMAIN_CLASS_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.FieldImpl <em>Field</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.FieldImpl
     * @see fw2.model2.impl.Model2PackageImpl#getField()
     * @generated
     */
	int FIELD = 24;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__ANNOTATIONS = VIEW_PRIMITIVE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__NAME = VIEW_PRIMITIVE__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__ID = VIEW_PRIMITIVE__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__LABEL = VIEW_PRIMITIVE__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__TYPE = VIEW_PRIMITIVE__TYPE;

	/**
     * The feature id for the '<em><b>Mandatory</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__MANDATORY = VIEW_PRIMITIVE_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Max Length</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__MAX_LENGTH = VIEW_PRIMITIVE_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Reference List Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__REFERENCE_LIST_NAME = VIEW_PRIMITIVE_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>Row Index</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__ROW_INDEX = VIEW_PRIMITIVE_FEATURE_COUNT + 3;

	/**
     * The feature id for the '<em><b>Select Disabled</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__SELECT_DISABLED = VIEW_PRIMITIVE_FEATURE_COUNT + 4;

	/**
     * The feature id for the '<em><b>Select Header Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__SELECT_HEADER_VALUE = VIEW_PRIMITIVE_FEATURE_COUNT + 5;

	/**
     * The feature id for the '<em><b>Size</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__SIZE = VIEW_PRIMITIVE_FEATURE_COUNT + 6;

	/**
     * The feature id for the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD__PRESENT_IN_STAGING = VIEW_PRIMITIVE_FEATURE_COUNT + 7;

	/**
     * The feature id for the '<em><b>Comments</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int FIELD__COMMENTS = VIEW_PRIMITIVE_FEATURE_COUNT + 8;

    /**
     * The feature id for the '<em><b>Mapping</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int FIELD__MAPPING = VIEW_PRIMITIVE_FEATURE_COUNT + 9;

    /**
     * The number of structural features of the '<em>Field</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_FEATURE_COUNT = VIEW_PRIMITIVE_FEATURE_COUNT + 10;

	/**
     * The number of operations of the '<em>Field</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_OPERATION_COUNT = VIEW_PRIMITIVE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.FieldExtensionImpl <em>Field Extension</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.FieldExtensionImpl
     * @see fw2.model2.impl.Model2PackageImpl#getFieldExtension()
     * @generated
     */
	int FIELD_EXTENSION = 25;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__ANNOTATIONS = FIELD__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__NAME = FIELD__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__ID = FIELD__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__LABEL = FIELD__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__TYPE = FIELD__TYPE;

	/**
     * The feature id for the '<em><b>Mandatory</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__MANDATORY = FIELD__MANDATORY;

	/**
     * The feature id for the '<em><b>Max Length</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__MAX_LENGTH = FIELD__MAX_LENGTH;

	/**
     * The feature id for the '<em><b>Reference List Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__REFERENCE_LIST_NAME = FIELD__REFERENCE_LIST_NAME;

	/**
     * The feature id for the '<em><b>Row Index</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__ROW_INDEX = FIELD__ROW_INDEX;

	/**
     * The feature id for the '<em><b>Select Disabled</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__SELECT_DISABLED = FIELD__SELECT_DISABLED;

	/**
     * The feature id for the '<em><b>Select Header Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__SELECT_HEADER_VALUE = FIELD__SELECT_HEADER_VALUE;

	/**
     * The feature id for the '<em><b>Size</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__SIZE = FIELD__SIZE;

	/**
     * The feature id for the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__PRESENT_IN_STAGING = FIELD__PRESENT_IN_STAGING;

	/**
     * The feature id for the '<em><b>Comments</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int FIELD_EXTENSION__COMMENTS = FIELD__COMMENTS;

    /**
     * The feature id for the '<em><b>Mapping</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int FIELD_EXTENSION__MAPPING = FIELD__MAPPING;

    /**
     * The feature id for the '<em><b>Extension</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION__EXTENSION = FIELD_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Field Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION_FEATURE_COUNT = FIELD_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Field Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_EXTENSION_OPERATION_COUNT = FIELD_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.FieldGroupImpl <em>Field Group</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.FieldGroupImpl
     * @see fw2.model2.impl.Model2PackageImpl#getFieldGroup()
     * @generated
     */
	int FIELD_GROUP = 26;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__ANNOTATIONS = VIEW_COMPONENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__NAME = VIEW_COMPONENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__ID = VIEW_COMPONENT__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__LABEL = VIEW_COMPONENT__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__TYPE = VIEW_COMPONENT__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__COMPONENT_ID = VIEW_COMPONENT__COMPONENT_ID;

	/**
     * The feature id for the '<em><b>Actions</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__ACTIONS = VIEW_COMPONENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Fields</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__FIELDS = VIEW_COMPONENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Display Order</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__DISPLAY_ORDER = VIEW_COMPONENT_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP__PRESENT_IN_STAGING = VIEW_COMPONENT_FEATURE_COUNT + 3;

	/**
     * The number of structural features of the '<em>Field Group</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_FEATURE_COUNT = VIEW_COMPONENT_FEATURE_COUNT + 4;

	/**
     * The number of operations of the '<em>Field Group</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_OPERATION_COUNT = VIEW_COMPONENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.FieldGroupExtensionImpl <em>Field Group Extension</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.FieldGroupExtensionImpl
     * @see fw2.model2.impl.Model2PackageImpl#getFieldGroupExtension()
     * @generated
     */
	int FIELD_GROUP_EXTENSION = 27;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__ANNOTATIONS = FIELD_GROUP__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__NAME = FIELD_GROUP__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__ID = FIELD_GROUP__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__LABEL = FIELD_GROUP__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__TYPE = FIELD_GROUP__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__COMPONENT_ID = FIELD_GROUP__COMPONENT_ID;

	/**
     * The feature id for the '<em><b>Actions</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__ACTIONS = FIELD_GROUP__ACTIONS;

	/**
     * The feature id for the '<em><b>Fields</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__FIELDS = FIELD_GROUP__FIELDS;

	/**
     * The feature id for the '<em><b>Display Order</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__DISPLAY_ORDER = FIELD_GROUP__DISPLAY_ORDER;

	/**
     * The feature id for the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__PRESENT_IN_STAGING = FIELD_GROUP__PRESENT_IN_STAGING;

	/**
     * The feature id for the '<em><b>Extension</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION__EXTENSION = FIELD_GROUP_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Field Group Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION_FEATURE_COUNT = FIELD_GROUP_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Field Group Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int FIELD_GROUP_EXTENSION_OPERATION_COUNT = FIELD_GROUP_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ReferenceImpl <em>Reference</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ReferenceImpl
     * @see fw2.model2.impl.Model2PackageImpl#getReference()
     * @generated
     */
	int REFERENCE = 34;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE__ANNOTATIONS = ASSOCIATION__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE__NAME = ASSOCIATION__NAME;

	/**
     * The feature id for the '<em><b>Cardinality</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE__CARDINALITY = ASSOCIATION__CARDINALITY;

	/**
     * The feature id for the '<em><b>Related Class Orm</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE__RELATED_CLASS_ORM = ASSOCIATION__RELATED_CLASS_ORM;

	/**
     * The feature id for the '<em><b>Relation</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE__RELATION = ASSOCIATION_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Reference</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE_FEATURE_COUNT = ASSOCIATION_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Reference</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int REFERENCE_OPERATION_COUNT = ASSOCIATION_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.LookupImpl <em>Lookup</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.LookupImpl
     * @see fw2.model2.impl.Model2PackageImpl#getLookup()
     * @generated
     */
	int LOOKUP = 28;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP__ANNOTATIONS = REFERENCE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP__NAME = REFERENCE__NAME;

	/**
     * The feature id for the '<em><b>Cardinality</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP__CARDINALITY = REFERENCE__CARDINALITY;

	/**
     * The feature id for the '<em><b>Related Class Orm</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP__RELATED_CLASS_ORM = REFERENCE__RELATED_CLASS_ORM;

	/**
     * The feature id for the '<em><b>Relation</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP__RELATION = REFERENCE__RELATION;

	/**
     * The number of structural features of the '<em>Lookup</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP_FEATURE_COUNT = REFERENCE_FEATURE_COUNT + 0;

	/**
     * The number of operations of the '<em>Lookup</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int LOOKUP_OPERATION_COUNT = REFERENCE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ModelImpl <em>Model</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ModelImpl
     * @see fw2.model2.impl.Model2PackageImpl#getModel()
     * @generated
     */
	int MODEL = 29;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Elements</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL__ELEMENTS = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Model</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Model</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int MODEL_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.PrimitiveExtensionImpl <em>Primitive Extension</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.PrimitiveExtensionImpl
     * @see fw2.model2.impl.Model2PackageImpl#getPrimitiveExtension()
     * @generated
     */
	int PRIMITIVE_EXTENSION = 32;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_EXTENSION__ANNOTATIONS = PRIMITIVE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_EXTENSION__NAME = PRIMITIVE__NAME;

	/**
     * The feature id for the '<em><b>Extension</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_EXTENSION__EXTENSION = PRIMITIVE_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Primitive Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_EXTENSION_FEATURE_COUNT = PRIMITIVE_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Primitive Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PRIMITIVE_EXTENSION_OPERATION_COUNT = PRIMITIVE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.PropertyImpl <em>Property</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.PropertyImpl
     * @see fw2.model2.impl.Model2PackageImpl#getProperty()
     * @generated
     */
	int PROPERTY = 33;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PROPERTY__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PROPERTY__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Value</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PROPERTY__VALUE = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The number of structural features of the '<em>Property</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PROPERTY_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of operations of the '<em>Property</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int PROPERTY_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.SummaryTableImpl <em>Summary Table</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.SummaryTableImpl
     * @see fw2.model2.impl.Model2PackageImpl#getSummaryTable()
     * @generated
     */
	int SUMMARY_TABLE = 35;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__ANNOTATIONS = VIEW_COMPONENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__NAME = VIEW_COMPONENT__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__ID = VIEW_COMPONENT__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__LABEL = VIEW_COMPONENT__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__TYPE = VIEW_COMPONENT__TYPE;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__COMPONENT_ID = VIEW_COMPONENT__COMPONENT_ID;

	/**
     * The feature id for the '<em><b>Columns</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__COLUMNS = VIEW_COMPONENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Domain Wrapper</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__DOMAIN_WRAPPER = VIEW_COMPONENT_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Bap Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE__BAP_NAME = VIEW_COMPONENT_FEATURE_COUNT + 2;

	/**
     * The number of structural features of the '<em>Summary Table</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE_FEATURE_COUNT = VIEW_COMPONENT_FEATURE_COUNT + 3;

	/**
     * The number of operations of the '<em>Summary Table</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int SUMMARY_TABLE_OPERATION_COUNT = VIEW_COMPONENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.TableColumnImpl <em>Table Column</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.TableColumnImpl
     * @see fw2.model2.impl.Model2PackageImpl#getTableColumn()
     * @generated
     */
	int TABLE_COLUMN = 36;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__ANNOTATIONS = VIEW_PRIMITIVE__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__NAME = VIEW_PRIMITIVE__NAME;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__ID = VIEW_PRIMITIVE__ID;

	/**
     * The feature id for the '<em><b>Label</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__LABEL = VIEW_PRIMITIVE__LABEL;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__TYPE = VIEW_PRIMITIVE__TYPE;

	/**
     * The feature id for the '<em><b>Header Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__HEADER_VALUE = VIEW_PRIMITIVE_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Index</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__INDEX = VIEW_PRIMITIVE_FEATURE_COUNT + 1;

	/**
     * The feature id for the '<em><b>Property</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__PROPERTY = VIEW_PRIMITIVE_FEATURE_COUNT + 2;

	/**
     * The feature id for the '<em><b>Reference List Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__REFERENCE_LIST_NAME = VIEW_PRIMITIVE_FEATURE_COUNT + 3;

	/**
     * The feature id for the '<em><b>Select Disabled</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN__SELECT_DISABLED = VIEW_PRIMITIVE_FEATURE_COUNT + 4;

	/**
     * The feature id for the '<em><b>Comments</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int TABLE_COLUMN__COMMENTS = VIEW_PRIMITIVE_FEATURE_COUNT + 5;

    /**
     * The feature id for the '<em><b>Mapping</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int TABLE_COLUMN__MAPPING = VIEW_PRIMITIVE_FEATURE_COUNT + 6;

    /**
     * The number of structural features of the '<em>Table Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN_FEATURE_COUNT = VIEW_PRIMITIVE_FEATURE_COUNT + 7;

	/**
     * The number of operations of the '<em>Table Column</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TABLE_COLUMN_OPERATION_COUNT = VIEW_PRIMITIVE_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.TagImpl <em>Tag</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.TagImpl
     * @see fw2.model2.impl.Model2PackageImpl#getTag()
     * @generated
     */
	int TAG = 37;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TAG__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TAG__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TAG__TYPE = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TAG__VALUE = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Tag</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TAG_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Tag</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TAG_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.TypeExtensionImpl <em>Type Extension</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.TypeExtensionImpl
     * @see fw2.model2.impl.Model2PackageImpl#getTypeExtension()
     * @generated
     */
	int TYPE_EXTENSION = 38;

	/**
     * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TYPE_EXTENSION__ANNOTATIONS = MODEL_ELEMENT__ANNOTATIONS;

	/**
     * The feature id for the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TYPE_EXTENSION__NAME = MODEL_ELEMENT__NAME;

	/**
     * The feature id for the '<em><b>Domain Class</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TYPE_EXTENSION__DOMAIN_CLASS = MODEL_ELEMENT_FEATURE_COUNT + 0;

	/**
     * The feature id for the '<em><b>Properties</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TYPE_EXTENSION__PROPERTIES = MODEL_ELEMENT_FEATURE_COUNT + 1;

	/**
     * The number of structural features of the '<em>Type Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TYPE_EXTENSION_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 2;

	/**
     * The number of operations of the '<em>Type Extension</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int TYPE_EXTENSION_OPERATION_COUNT = MODEL_ELEMENT_OPERATION_COUNT + 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.UiFieldTypeImpl <em>Ui Field Type</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.UiFieldTypeImpl
     * @see fw2.model2.impl.Model2PackageImpl#getUiFieldType()
     * @generated
     */
	int UI_FIELD_TYPE = 42;

	/**
     * The feature id for the '<em><b>Data Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE__DATA_TYPE = 0;

	/**
     * The feature id for the '<em><b>Html Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE__HTML_TYPE = 1;

	/**
     * The feature id for the '<em><b>On Screen Validation Method</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD = 2;

	/**
     * The feature id for the '<em><b>Properties</b></em>' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE__PROPERTIES = 3;

	/**
     * The number of structural features of the '<em>Ui Field Type</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_FEATURE_COUNT = 4;

	/**
     * The number of operations of the '<em>Ui Field Type</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_OPERATION_COUNT = 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.UiFieldTypePropertyImpl <em>Ui Field Type Property</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.UiFieldTypePropertyImpl
     * @see fw2.model2.impl.Model2PackageImpl#getUiFieldTypeProperty()
     * @generated
     */
	int UI_FIELD_TYPE_PROPERTY = 43;

	/**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME = 0;

	/**
     * The feature id for the '<em><b>Property Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE = 1;

	/**
     * The feature id for the '<em><b>Type Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_PROPERTY__TYPE_NAME = 2;

	/**
     * The number of structural features of the '<em>Ui Field Type Property</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_PROPERTY_FEATURE_COUNT = 3;

	/**
     * The number of operations of the '<em>Ui Field Type Property</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int UI_FIELD_TYPE_PROPERTY_OPERATION_COUNT = 0;

	/**
     * The meta object id for the '{@link fw2.model2.impl.ComponentMappingImpl <em>Component Mapping</em>}' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.impl.ComponentMappingImpl
     * @see fw2.model2.impl.Model2PackageImpl#getComponentMapping()
     * @generated
     */
	int COMPONENT_MAPPING = 44;

	/**
     * The feature id for the '<em><b>Class Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_MAPPING__CLASS_NAME = 0;

	/**
     * The feature id for the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_MAPPING__COMPONENT_ID = 1;

	/**
     * The feature id for the '<em><b>Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_MAPPING__ID = 2;

	/**
     * The feature id for the '<em><b>Type Code</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_MAPPING__TYPE_CODE = 3;

	/**
     * The number of structural features of the '<em>Component Mapping</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_MAPPING_FEATURE_COUNT = 4;

	/**
     * The number of operations of the '<em>Component Mapping</em>' class.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	int COMPONENT_MAPPING_OPERATION_COUNT = 0;

	/**
     * The meta object id for the '{@link fw2.model2.CardinalityTypeEnum <em>Cardinality Type Enum</em>}' enum.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.CardinalityTypeEnum
     * @see fw2.model2.impl.Model2PackageImpl#getCardinalityTypeEnum()
     * @generated
     */
	int CARDINALITY_TYPE_ENUM = 45;

	/**
     * The meta object id for the '{@link fw2.model2.DbColumnTypeEnum <em>Db Column Type Enum</em>}' enum.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.DbColumnTypeEnum
     * @see fw2.model2.impl.Model2PackageImpl#getDbColumnTypeEnum()
     * @generated
     */
	int DB_COLUMN_TYPE_ENUM = 46;

	/**
     * The meta object id for the '{@link fw2.model2.FieldTypeEnum <em>Field Type Enum</em>}' enum.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see fw2.model2.FieldTypeEnum
     * @see fw2.model2.impl.Model2PackageImpl#getFieldTypeEnum()
     * @generated
     */
	int FIELD_TYPE_ENUM = 47;


	/**
     * Returns the meta object for class '{@link fw2.model2.Action <em>Action</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Action</em>'.
     * @see fw2.model2.Action
     * @generated
     */
	EClass getAction();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Action#getAccessKey <em>Access Key</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Access Key</em>'.
     * @see fw2.model2.Action#getAccessKey()
     * @see #getAction()
     * @generated
     */
	EAttribute getAction_AccessKey();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Action#getUrl <em>Url</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Url</em>'.
     * @see fw2.model2.Action#getUrl()
     * @see #getAction()
     * @generated
     */
	EAttribute getAction_Url();

	/**
     * Returns the meta object for class '{@link fw2.model2.Aggregation <em>Aggregation</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Aggregation</em>'.
     * @see fw2.model2.Aggregation
     * @generated
     */
	EClass getAggregation();

	/**
     * Returns the meta object for the containment reference '{@link fw2.model2.Aggregation#getComponent <em>Component</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference '<em>Component</em>'.
     * @see fw2.model2.Aggregation#getComponent()
     * @see #getAggregation()
     * @generated
     */
	EReference getAggregation_Component();

	/**
     * Returns the meta object for class '{@link fw2.model2.Annotation <em>Annotation</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Annotation</em>'.
     * @see fw2.model2.Annotation
     * @generated
     */
	EClass getAnnotation();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Annotation#getTags <em>Tags</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Tags</em>'.
     * @see fw2.model2.Annotation#getTags()
     * @see #getAnnotation()
     * @generated
     */
	EReference getAnnotation_Tags();

	/**
     * Returns the meta object for class '{@link fw2.model2.Application <em>Application</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Application</em>'.
     * @see fw2.model2.Application
     * @generated
     */
	EClass getApplication();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Application#getBaps <em>Baps</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Baps</em>'.
     * @see fw2.model2.Application#getBaps()
     * @see #getApplication()
     * @generated
     */
	EReference getApplication_Baps();

	/**
     * Returns the meta object for class '{@link fw2.model2.Association <em>Association</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Association</em>'.
     * @see fw2.model2.Association
     * @generated
     */
	EClass getAssociation();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Association#getCardinality <em>Cardinality</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Cardinality</em>'.
     * @see fw2.model2.Association#getCardinality()
     * @see #getAssociation()
     * @generated
     */
	EAttribute getAssociation_Cardinality();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.Association#getRelatedClassOrm <em>Related Class Orm</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Related Class Orm</em>'.
     * @see fw2.model2.Association#getRelatedClassOrm()
     * @see #getAssociation()
     * @generated
     */
	EReference getAssociation_RelatedClassOrm();

	/**
     * Returns the meta object for class '{@link fw2.model2.Attribute <em>Attribute</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Attribute</em>'.
     * @see fw2.model2.Attribute
     * @generated
     */
	EClass getAttribute();

	/**
     * Returns the meta object for class '{@link fw2.model2.Bap <em>Bap</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Bap</em>'.
     * @see fw2.model2.Bap
     * @generated
     */
	EClass getBap();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Bap#getComponentMapping <em>Component Mapping</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Component Mapping</em>'.
     * @see fw2.model2.Bap#getComponentMapping()
     * @see #getBap()
     * @generated
     */
	EReference getBap_ComponentMapping();

	/**
     * Returns the meta object for class '{@link fw2.model2.ClassOrm <em>Class Orm</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Class Orm</em>'.
     * @see fw2.model2.ClassOrm
     * @generated
     */
	EClass getClassOrm();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.ClassOrm#getAggregations <em>Aggregations</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Aggregations</em>'.
     * @see fw2.model2.ClassOrm#getAggregations()
     * @see #getClassOrm()
     * @generated
     */
	EReference getClassOrm_Aggregations();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.ClassOrm#getDataSet <em>Data Set</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Data Set</em>'.
     * @see fw2.model2.ClassOrm#getDataSet()
     * @see #getClassOrm()
     * @generated
     */
	EReference getClassOrm_DataSet();

	/**
     * Returns the meta object for the containment reference '{@link fw2.model2.ClassOrm#getKey <em>Key</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference '<em>Key</em>'.
     * @see fw2.model2.ClassOrm#getKey()
     * @see #getClassOrm()
     * @generated
     */
	EReference getClassOrm_Key();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.ClassOrm#getReferences <em>References</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>References</em>'.
     * @see fw2.model2.ClassOrm#getReferences()
     * @see #getClassOrm()
     * @generated
     */
	EReference getClassOrm_References();

	/**
     * Returns the meta object for class '{@link fw2.model2.Component <em>Component</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Component</em>'.
     * @see fw2.model2.Component
     * @generated
     */
	EClass getComponent();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Component#getLookups <em>Lookups</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Lookups</em>'.
     * @see fw2.model2.Component#getLookups()
     * @see #getComponent()
     * @generated
     */
	EReference getComponent_Lookups();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Component#getPrimitives <em>Primitives</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Primitives</em>'.
     * @see fw2.model2.Component#getPrimitives()
     * @see #getComponent()
     * @generated
     */
	EReference getComponent_Primitives();

	/**
     * Returns the meta object for class '{@link fw2.model2.Constant <em>Constant</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Constant</em>'.
     * @see fw2.model2.Constant
     * @generated
     */
	EClass getConstant();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Constant#getType <em>Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Type</em>'.
     * @see fw2.model2.Constant#getType()
     * @see #getConstant()
     * @generated
     */
	EAttribute getConstant_Type();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Constant#getValue <em>Value</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Value</em>'.
     * @see fw2.model2.Constant#getValue()
     * @see #getConstant()
     * @generated
     */
	EAttribute getConstant_Value();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbColumn <em>Db Column</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Column</em>'.
     * @see fw2.model2.DbColumn
     * @generated
     */
	EClass getDbColumn();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getDefault <em>Default</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Default</em>'.
     * @see fw2.model2.DbColumn#getDefault()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Default();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getKeySeq <em>Key Seq</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Key Seq</em>'.
     * @see fw2.model2.DbColumn#getKeySeq()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_KeySeq();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getLength <em>Length</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Length</em>'.
     * @see fw2.model2.DbColumn#getLength()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Length();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getNullable <em>Nullable</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Nullable</em>'.
     * @see fw2.model2.DbColumn#getNullable()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Nullable();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getScale <em>Scale</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Scale</em>'.
     * @see fw2.model2.DbColumn#getScale()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Scale();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getSeq <em>Seq</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Seq</em>'.
     * @see fw2.model2.DbColumn#getSeq()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Seq();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getSize <em>Size</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Size</em>'.
     * @see fw2.model2.DbColumn#getSize()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Size();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumn#getType <em>Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Type</em>'.
     * @see fw2.model2.DbColumn#getType()
     * @see #getDbColumn()
     * @generated
     */
	EAttribute getDbColumn_Type();

	/**
     * Returns the meta object for the container reference '{@link fw2.model2.DbColumn#getTable <em>Table</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the container reference '<em>Table</em>'.
     * @see fw2.model2.DbColumn#getTable()
     * @see #getDbColumn()
     * @generated
     */
	EReference getDbColumn_Table();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbColumnMap <em>Db Column Map</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Column Map</em>'.
     * @see fw2.model2.DbColumnMap
     * @generated
     */
	EClass getDbColumnMap();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbColumnMap#getDbColumnName <em>Db Column Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Db Column Name</em>'.
     * @see fw2.model2.DbColumnMap#getDbColumnName()
     * @see #getDbColumnMap()
     * @generated
     */
	EAttribute getDbColumnMap_DbColumnName();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbDataSet <em>Db Data Set</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Data Set</em>'.
     * @see fw2.model2.DbDataSet
     * @generated
     */
	EClass getDbDataSet();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbForeignKeyColumn <em>Db Foreign Key Column</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Foreign Key Column</em>'.
     * @see fw2.model2.DbForeignKeyColumn
     * @generated
     */
	EClass getDbForeignKeyColumn();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbForeignKeyColumn#getKeySeq <em>Key Seq</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Key Seq</em>'.
     * @see fw2.model2.DbForeignKeyColumn#getKeySeq()
     * @see #getDbForeignKeyColumn()
     * @generated
     */
	EAttribute getDbForeignKeyColumn_KeySeq();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DbForeignKeyColumn#getSeq <em>Seq</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Seq</em>'.
     * @see fw2.model2.DbForeignKeyColumn#getSeq()
     * @see #getDbForeignKeyColumn()
     * @generated
     */
	EAttribute getDbForeignKeyColumn_Seq();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbJoinKey <em>Db Join Key</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Join Key</em>'.
     * @see fw2.model2.DbJoinKey
     * @generated
     */
	EClass getDbJoinKey();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbJoinParameter <em>Db Join Parameter</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Join Parameter</em>'.
     * @see fw2.model2.DbJoinParameter
     * @generated
     */
	EClass getDbJoinParameter();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbRelation <em>Db Relation</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Relation</em>'.
     * @see fw2.model2.DbRelation
     * @generated
     */
	EClass getDbRelation();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.DbRelation#getForeignKeyColumns <em>Foreign Key Columns</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Foreign Key Columns</em>'.
     * @see fw2.model2.DbRelation#getForeignKeyColumns()
     * @see #getDbRelation()
     * @generated
     */
	EReference getDbRelation_ForeignKeyColumns();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.DbRelation#getRefTable <em>Ref Table</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Ref Table</em>'.
     * @see fw2.model2.DbRelation#getRefTable()
     * @see #getDbRelation()
     * @generated
     */
	EReference getDbRelation_RefTable();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.DbRelation#getTable <em>Table</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Table</em>'.
     * @see fw2.model2.DbRelation#getTable()
     * @see #getDbRelation()
     * @generated
     */
	EReference getDbRelation_Table();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbTable <em>Db Table</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db Table</em>'.
     * @see fw2.model2.DbTable
     * @generated
     */
	EClass getDbTable();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.DbTable#getColumns <em>Columns</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Columns</em>'.
     * @see fw2.model2.DbTable#getColumns()
     * @see #getDbTable()
     * @generated
     */
	EReference getDbTable_Columns();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbView <em>Db View</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db View</em>'.
     * @see fw2.model2.DbView
     * @generated
     */
	EClass getDbView();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.DbView#getJoinKeys <em>Join Keys</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Join Keys</em>'.
     * @see fw2.model2.DbView#getJoinKeys()
     * @see #getDbView()
     * @generated
     */
	EReference getDbView_JoinKeys();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.DbView#getJoinParameters <em>Join Parameters</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Join Parameters</em>'.
     * @see fw2.model2.DbView#getJoinParameters()
     * @see #getDbView()
     * @generated
     */
	EReference getDbView_JoinParameters();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.DbView#getViewColumns <em>View Columns</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>View Columns</em>'.
     * @see fw2.model2.DbView#getViewColumns()
     * @see #getDbView()
     * @generated
     */
	EReference getDbView_ViewColumns();

	/**
     * Returns the meta object for class '{@link fw2.model2.DbViewColumn <em>Db View Column</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Db View Column</em>'.
     * @see fw2.model2.DbViewColumn
     * @generated
     */
	EClass getDbViewColumn();

	/**
     * Returns the meta object for class '{@link fw2.model2.DiscriminatorKey <em>Discriminator Key</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Discriminator Key</em>'.
     * @see fw2.model2.DiscriminatorKey
     * @generated
     */
	EClass getDiscriminatorKey();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DiscriminatorKey#getDiscriminatorValue <em>Discriminator Value</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Discriminator Value</em>'.
     * @see fw2.model2.DiscriminatorKey#getDiscriminatorValue()
     * @see #getDiscriminatorKey()
     * @generated
     */
	EAttribute getDiscriminatorKey_DiscriminatorValue();

	/**
     * Returns the meta object for class '{@link fw2.model2.DomainAttribute <em>Domain Attribute</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Domain Attribute</em>'.
     * @see fw2.model2.DomainAttribute
     * @generated
     */
	EClass getDomainAttribute();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.DomainAttribute#getDataType <em>Data Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Data Type</em>'.
     * @see fw2.model2.DomainAttribute#getDataType()
     * @see #getDomainAttribute()
     * @generated
     */
	EReference getDomainAttribute_DataType();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DomainAttribute#isMultivalued <em>Multivalued</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Multivalued</em>'.
     * @see fw2.model2.DomainAttribute#isMultivalued()
     * @see #getDomainAttribute()
     * @generated
     */
	EAttribute getDomainAttribute_Multivalued();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DomainAttribute#isReference <em>Reference</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Reference</em>'.
     * @see fw2.model2.DomainAttribute#isReference()
     * @see #getDomainAttribute()
     * @generated
     */
	EAttribute getDomainAttribute_Reference();

	/**
     * Returns the meta object for class '{@link fw2.model2.DomainClass <em>Domain Class</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Domain Class</em>'.
     * @see fw2.model2.DomainClass
     * @generated
     */
	EClass getDomainClass();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.DomainClass#getSuperclass <em>Superclass</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Superclass</em>'.
     * @see fw2.model2.DomainClass#getSuperclass()
     * @see #getDomainClass()
     * @generated
     */
	EReference getDomainClass_Superclass();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.DomainClass#getPlatformClass <em>Platform Class</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Platform Class</em>'.
     * @see fw2.model2.DomainClass#getPlatformClass()
     * @see #getDomainClass()
     * @generated
     */
	EReference getDomainClass_PlatformClass();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.DomainClass#getAttributes <em>Attributes</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Attributes</em>'.
     * @see fw2.model2.DomainClass#getAttributes()
     * @see #getDomainClass()
     * @generated
     */
	EReference getDomainClass_Attributes();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DomainClass#isAbstract <em>Abstract</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Abstract</em>'.
     * @see fw2.model2.DomainClass#isAbstract()
     * @see #getDomainClass()
     * @generated
     */
	EAttribute getDomainClass_Abstract();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.DomainClass#isPrimitive <em>Primitive</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Primitive</em>'.
     * @see fw2.model2.DomainClass#isPrimitive()
     * @see #getDomainClass()
     * @generated
     */
	EAttribute getDomainClass_Primitive();

	/**
     * Returns the meta object for class '{@link fw2.model2.Editor <em>Editor</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Editor</em>'.
     * @see fw2.model2.Editor
     * @generated
     */
	EClass getEditor();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Editor#getEditors <em>Editors</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Editors</em>'.
     * @see fw2.model2.Editor#getEditors()
     * @see #getEditor()
     * @generated
     */
	EReference getEditor_Editors();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Editor#getFieldGroups <em>Field Groups</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Field Groups</em>'.
     * @see fw2.model2.Editor#getFieldGroups()
     * @see #getEditor()
     * @generated
     */
	EReference getEditor_FieldGroups();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Editor#getSummaryTables <em>Summary Tables</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Summary Tables</em>'.
     * @see fw2.model2.Editor#getSummaryTables()
     * @see #getEditor()
     * @generated
     */
	EReference getEditor_SummaryTables();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Editor#getPresentInStaging <em>Present In Staging</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Present In Staging</em>'.
     * @see fw2.model2.Editor#getPresentInStaging()
     * @see #getEditor()
     * @generated
     */
	EAttribute getEditor_PresentInStaging();

	/**
     * Returns the meta object for class '{@link fw2.model2.Field <em>Field</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Field</em>'.
     * @see fw2.model2.Field
     * @generated
     */
	EClass getField();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#isMandatory <em>Mandatory</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Mandatory</em>'.
     * @see fw2.model2.Field#isMandatory()
     * @see #getField()
     * @generated
     */
	EAttribute getField_Mandatory();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getMaxLength <em>Max Length</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Max Length</em>'.
     * @see fw2.model2.Field#getMaxLength()
     * @see #getField()
     * @generated
     */
	EAttribute getField_MaxLength();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getReferenceListName <em>Reference List Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Reference List Name</em>'.
     * @see fw2.model2.Field#getReferenceListName()
     * @see #getField()
     * @generated
     */
	EAttribute getField_ReferenceListName();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getRowIndex <em>Row Index</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Row Index</em>'.
     * @see fw2.model2.Field#getRowIndex()
     * @see #getField()
     * @generated
     */
	EAttribute getField_RowIndex();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getSelectDisabled <em>Select Disabled</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Select Disabled</em>'.
     * @see fw2.model2.Field#getSelectDisabled()
     * @see #getField()
     * @generated
     */
	EAttribute getField_SelectDisabled();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getSelectHeaderValue <em>Select Header Value</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Select Header Value</em>'.
     * @see fw2.model2.Field#getSelectHeaderValue()
     * @see #getField()
     * @generated
     */
	EAttribute getField_SelectHeaderValue();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getSize <em>Size</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Size</em>'.
     * @see fw2.model2.Field#getSize()
     * @see #getField()
     * @generated
     */
	EAttribute getField_Size();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getPresentInStaging <em>Present In Staging</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Present In Staging</em>'.
     * @see fw2.model2.Field#getPresentInStaging()
     * @see #getField()
     * @generated
     */
	EAttribute getField_PresentInStaging();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getComments <em>Comments</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Comments</em>'.
     * @see fw2.model2.Field#getComments()
     * @see #getField()
     * @generated
     */
    EAttribute getField_Comments();

    /**
     * Returns the meta object for the attribute '{@link fw2.model2.Field#getMapping <em>Mapping</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Mapping</em>'.
     * @see fw2.model2.Field#getMapping()
     * @see #getField()
     * @generated
     */
    EAttribute getField_Mapping();

    /**
     * Returns the meta object for class '{@link fw2.model2.FieldExtension <em>Field Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Field Extension</em>'.
     * @see fw2.model2.FieldExtension
     * @generated
     */
	EClass getFieldExtension();

	/**
     * Returns the meta object for the containment reference '{@link fw2.model2.FieldExtension#getExtension <em>Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference '<em>Extension</em>'.
     * @see fw2.model2.FieldExtension#getExtension()
     * @see #getFieldExtension()
     * @generated
     */
	EReference getFieldExtension_Extension();

	/**
     * Returns the meta object for class '{@link fw2.model2.FieldGroup <em>Field Group</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Field Group</em>'.
     * @see fw2.model2.FieldGroup
     * @generated
     */
	EClass getFieldGroup();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.FieldGroup#getActions <em>Actions</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Actions</em>'.
     * @see fw2.model2.FieldGroup#getActions()
     * @see #getFieldGroup()
     * @generated
     */
	EReference getFieldGroup_Actions();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.FieldGroup#getFields <em>Fields</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Fields</em>'.
     * @see fw2.model2.FieldGroup#getFields()
     * @see #getFieldGroup()
     * @generated
     */
	EReference getFieldGroup_Fields();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.FieldGroup#getDisplayOrder <em>Display Order</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Display Order</em>'.
     * @see fw2.model2.FieldGroup#getDisplayOrder()
     * @see #getFieldGroup()
     * @generated
     */
	EAttribute getFieldGroup_DisplayOrder();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.FieldGroup#getPresentInStaging <em>Present In Staging</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Present In Staging</em>'.
     * @see fw2.model2.FieldGroup#getPresentInStaging()
     * @see #getFieldGroup()
     * @generated
     */
	EAttribute getFieldGroup_PresentInStaging();

	/**
     * Returns the meta object for class '{@link fw2.model2.FieldGroupExtension <em>Field Group Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Field Group Extension</em>'.
     * @see fw2.model2.FieldGroupExtension
     * @generated
     */
	EClass getFieldGroupExtension();

	/**
     * Returns the meta object for the containment reference '{@link fw2.model2.FieldGroupExtension#getExtension <em>Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference '<em>Extension</em>'.
     * @see fw2.model2.FieldGroupExtension#getExtension()
     * @see #getFieldGroupExtension()
     * @generated
     */
	EReference getFieldGroupExtension_Extension();

	/**
     * Returns the meta object for class '{@link fw2.model2.Lookup <em>Lookup</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Lookup</em>'.
     * @see fw2.model2.Lookup
     * @generated
     */
	EClass getLookup();

	/**
     * Returns the meta object for class '{@link fw2.model2.Model <em>Model</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Model</em>'.
     * @see fw2.model2.Model
     * @generated
     */
	EClass getModel();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.Model#getElements <em>Elements</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Elements</em>'.
     * @see fw2.model2.Model#getElements()
     * @see #getModel()
     * @generated
     */
	EReference getModel_Elements();

	/**
     * Returns the meta object for class '{@link fw2.model2.ModelElement <em>Model Element</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Model Element</em>'.
     * @see fw2.model2.ModelElement
     * @generated
     */
	EClass getModelElement();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.ModelElement#getAnnotations <em>Annotations</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Annotations</em>'.
     * @see fw2.model2.ModelElement#getAnnotations()
     * @see #getModelElement()
     * @generated
     */
	EReference getModelElement_Annotations();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ModelElement#getName <em>Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Name</em>'.
     * @see fw2.model2.ModelElement#getName()
     * @see #getModelElement()
     * @generated
     */
	EAttribute getModelElement_Name();

	/**
     * Returns the meta object for class '{@link fw2.model2.Primitive <em>Primitive</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Primitive</em>'.
     * @see fw2.model2.Primitive
     * @generated
     */
	EClass getPrimitive();

	/**
     * Returns the meta object for class '{@link fw2.model2.PrimitiveExtension <em>Primitive Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Primitive Extension</em>'.
     * @see fw2.model2.PrimitiveExtension
     * @generated
     */
	EClass getPrimitiveExtension();

	/**
     * Returns the meta object for the containment reference '{@link fw2.model2.PrimitiveExtension#getExtension <em>Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference '<em>Extension</em>'.
     * @see fw2.model2.PrimitiveExtension#getExtension()
     * @see #getPrimitiveExtension()
     * @generated
     */
	EReference getPrimitiveExtension_Extension();

	/**
     * Returns the meta object for class '{@link fw2.model2.Property <em>Property</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Property</em>'.
     * @see fw2.model2.Property
     * @generated
     */
	EClass getProperty();

	/**
     * Returns the meta object for the containment reference '{@link fw2.model2.Property#getValue <em>Value</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference '<em>Value</em>'.
     * @see fw2.model2.Property#getValue()
     * @see #getProperty()
     * @generated
     */
	EReference getProperty_Value();

	/**
     * Returns the meta object for class '{@link fw2.model2.Reference <em>Reference</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Reference</em>'.
     * @see fw2.model2.Reference
     * @generated
     */
	EClass getReference();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.Reference#getRelation <em>Relation</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Relation</em>'.
     * @see fw2.model2.Reference#getRelation()
     * @see #getReference()
     * @generated
     */
	EReference getReference_Relation();

	/**
     * Returns the meta object for class '{@link fw2.model2.SummaryTable <em>Summary Table</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Summary Table</em>'.
     * @see fw2.model2.SummaryTable
     * @generated
     */
	EClass getSummaryTable();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.SummaryTable#getColumns <em>Columns</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Columns</em>'.
     * @see fw2.model2.SummaryTable#getColumns()
     * @see #getSummaryTable()
     * @generated
     */
	EReference getSummaryTable_Columns();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.SummaryTable#getDomainWrapper <em>Domain Wrapper</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Domain Wrapper</em>'.
     * @see fw2.model2.SummaryTable#getDomainWrapper()
     * @see #getSummaryTable()
     * @generated
     */
	EAttribute getSummaryTable_DomainWrapper();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.SummaryTable#getBapName <em>Bap Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Bap Name</em>'.
     * @see fw2.model2.SummaryTable#getBapName()
     * @see #getSummaryTable()
     * @generated
     */
	EAttribute getSummaryTable_BapName();

	/**
     * Returns the meta object for class '{@link fw2.model2.TableColumn <em>Table Column</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Table Column</em>'.
     * @see fw2.model2.TableColumn
     * @generated
     */
	EClass getTableColumn();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getHeaderValue <em>Header Value</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Header Value</em>'.
     * @see fw2.model2.TableColumn#getHeaderValue()
     * @see #getTableColumn()
     * @generated
     */
	EAttribute getTableColumn_HeaderValue();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getIndex <em>Index</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Index</em>'.
     * @see fw2.model2.TableColumn#getIndex()
     * @see #getTableColumn()
     * @generated
     */
	EAttribute getTableColumn_Index();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getProperty <em>Property</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Property</em>'.
     * @see fw2.model2.TableColumn#getProperty()
     * @see #getTableColumn()
     * @generated
     */
	EAttribute getTableColumn_Property();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getReferenceListName <em>Reference List Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Reference List Name</em>'.
     * @see fw2.model2.TableColumn#getReferenceListName()
     * @see #getTableColumn()
     * @generated
     */
	EAttribute getTableColumn_ReferenceListName();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getSelectDisabled <em>Select Disabled</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Select Disabled</em>'.
     * @see fw2.model2.TableColumn#getSelectDisabled()
     * @see #getTableColumn()
     * @generated
     */
	EAttribute getTableColumn_SelectDisabled();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getComments <em>Comments</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Comments</em>'.
     * @see fw2.model2.TableColumn#getComments()
     * @see #getTableColumn()
     * @generated
     */
    EAttribute getTableColumn_Comments();

    /**
     * Returns the meta object for the attribute '{@link fw2.model2.TableColumn#getMapping <em>Mapping</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Mapping</em>'.
     * @see fw2.model2.TableColumn#getMapping()
     * @see #getTableColumn()
     * @generated
     */
    EAttribute getTableColumn_Mapping();

    /**
     * Returns the meta object for class '{@link fw2.model2.Tag <em>Tag</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Tag</em>'.
     * @see fw2.model2.Tag
     * @generated
     */
	EClass getTag();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Tag#getType <em>Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Type</em>'.
     * @see fw2.model2.Tag#getType()
     * @see #getTag()
     * @generated
     */
	EAttribute getTag_Type();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.Tag#getValue <em>Value</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Value</em>'.
     * @see fw2.model2.Tag#getValue()
     * @see #getTag()
     * @generated
     */
	EAttribute getTag_Value();

	/**
     * Returns the meta object for class '{@link fw2.model2.TypeExtension <em>Type Extension</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Type Extension</em>'.
     * @see fw2.model2.TypeExtension
     * @generated
     */
	EClass getTypeExtension();

	/**
     * Returns the meta object for the reference '{@link fw2.model2.TypeExtension#getDomainClass <em>Domain Class</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the reference '<em>Domain Class</em>'.
     * @see fw2.model2.TypeExtension#getDomainClass()
     * @see #getTypeExtension()
     * @generated
     */
	EReference getTypeExtension_DomainClass();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.TypeExtension#getProperties <em>Properties</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Properties</em>'.
     * @see fw2.model2.TypeExtension#getProperties()
     * @see #getTypeExtension()
     * @generated
     */
	EReference getTypeExtension_Properties();

	/**
     * Returns the meta object for class '{@link fw2.model2.ViewComponent <em>View Component</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>View Component</em>'.
     * @see fw2.model2.ViewComponent
     * @generated
     */
	EClass getViewComponent();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ViewComponent#getComponentId <em>Component Id</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Component Id</em>'.
     * @see fw2.model2.ViewComponent#getComponentId()
     * @see #getViewComponent()
     * @generated
     */
	EAttribute getViewComponent_ComponentId();

	/**
     * Returns the meta object for class '{@link fw2.model2.ViewElement <em>View Element</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>View Element</em>'.
     * @see fw2.model2.ViewElement
     * @generated
     */
	EClass getViewElement();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ViewElement#getId <em>Id</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Id</em>'.
     * @see fw2.model2.ViewElement#getId()
     * @see #getViewElement()
     * @generated
     */
	EAttribute getViewElement_Id();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ViewElement#getLabel <em>Label</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Label</em>'.
     * @see fw2.model2.ViewElement#getLabel()
     * @see #getViewElement()
     * @generated
     */
	EAttribute getViewElement_Label();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ViewElement#getType <em>Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Type</em>'.
     * @see fw2.model2.ViewElement#getType()
     * @see #getViewElement()
     * @generated
     */
	EAttribute getViewElement_Type();

	/**
     * Returns the meta object for class '{@link fw2.model2.ViewPrimitive <em>View Primitive</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>View Primitive</em>'.
     * @see fw2.model2.ViewPrimitive
     * @generated
     */
	EClass getViewPrimitive();

	/**
     * Returns the meta object for class '{@link fw2.model2.UiFieldType <em>Ui Field Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Ui Field Type</em>'.
     * @see fw2.model2.UiFieldType
     * @generated
     */
	EClass getUiFieldType();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.UiFieldType#getDataType <em>Data Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Data Type</em>'.
     * @see fw2.model2.UiFieldType#getDataType()
     * @see #getUiFieldType()
     * @generated
     */
	EAttribute getUiFieldType_DataType();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.UiFieldType#getHtmlType <em>Html Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Html Type</em>'.
     * @see fw2.model2.UiFieldType#getHtmlType()
     * @see #getUiFieldType()
     * @generated
     */
	EAttribute getUiFieldType_HtmlType();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.UiFieldType#getOnScreenValidationMethod <em>On Screen Validation Method</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>On Screen Validation Method</em>'.
     * @see fw2.model2.UiFieldType#getOnScreenValidationMethod()
     * @see #getUiFieldType()
     * @generated
     */
	EAttribute getUiFieldType_OnScreenValidationMethod();

	/**
     * Returns the meta object for the containment reference list '{@link fw2.model2.UiFieldType#getProperties <em>Properties</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the containment reference list '<em>Properties</em>'.
     * @see fw2.model2.UiFieldType#getProperties()
     * @see #getUiFieldType()
     * @generated
     */
	EReference getUiFieldType_Properties();

	/**
     * Returns the meta object for class '{@link fw2.model2.UiFieldTypeProperty <em>Ui Field Type Property</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Ui Field Type Property</em>'.
     * @see fw2.model2.UiFieldTypeProperty
     * @generated
     */
	EClass getUiFieldTypeProperty();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.UiFieldTypeProperty#getPropertyName <em>Property Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Property Name</em>'.
     * @see fw2.model2.UiFieldTypeProperty#getPropertyName()
     * @see #getUiFieldTypeProperty()
     * @generated
     */
	EAttribute getUiFieldTypeProperty_PropertyName();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.UiFieldTypeProperty#getPropertyType <em>Property Type</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Property Type</em>'.
     * @see fw2.model2.UiFieldTypeProperty#getPropertyType()
     * @see #getUiFieldTypeProperty()
     * @generated
     */
	EAttribute getUiFieldTypeProperty_PropertyType();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.UiFieldTypeProperty#getTypeName <em>Type Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Type Name</em>'.
     * @see fw2.model2.UiFieldTypeProperty#getTypeName()
     * @see #getUiFieldTypeProperty()
     * @generated
     */
	EAttribute getUiFieldTypeProperty_TypeName();

	/**
     * Returns the meta object for class '{@link fw2.model2.ComponentMapping <em>Component Mapping</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for class '<em>Component Mapping</em>'.
     * @see fw2.model2.ComponentMapping
     * @generated
     */
	EClass getComponentMapping();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ComponentMapping#getClassName <em>Class Name</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Class Name</em>'.
     * @see fw2.model2.ComponentMapping#getClassName()
     * @see #getComponentMapping()
     * @generated
     */
	EAttribute getComponentMapping_ClassName();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ComponentMapping#getComponentId <em>Component Id</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Component Id</em>'.
     * @see fw2.model2.ComponentMapping#getComponentId()
     * @see #getComponentMapping()
     * @generated
     */
	EAttribute getComponentMapping_ComponentId();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ComponentMapping#getId <em>Id</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Id</em>'.
     * @see fw2.model2.ComponentMapping#getId()
     * @see #getComponentMapping()
     * @generated
     */
	EAttribute getComponentMapping_Id();

	/**
     * Returns the meta object for the attribute '{@link fw2.model2.ComponentMapping#getTypeCode <em>Type Code</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Type Code</em>'.
     * @see fw2.model2.ComponentMapping#getTypeCode()
     * @see #getComponentMapping()
     * @generated
     */
	EAttribute getComponentMapping_TypeCode();

	/**
     * Returns the meta object for enum '{@link fw2.model2.CardinalityTypeEnum <em>Cardinality Type Enum</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for enum '<em>Cardinality Type Enum</em>'.
     * @see fw2.model2.CardinalityTypeEnum
     * @generated
     */
	EEnum getCardinalityTypeEnum();

	/**
     * Returns the meta object for enum '{@link fw2.model2.DbColumnTypeEnum <em>Db Column Type Enum</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for enum '<em>Db Column Type Enum</em>'.
     * @see fw2.model2.DbColumnTypeEnum
     * @generated
     */
	EEnum getDbColumnTypeEnum();

	/**
     * Returns the meta object for enum '{@link fw2.model2.FieldTypeEnum <em>Field Type Enum</em>}'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the meta object for enum '<em>Field Type Enum</em>'.
     * @see fw2.model2.FieldTypeEnum
     * @generated
     */
	EEnum getFieldTypeEnum();

	/**
     * Returns the factory that creates the instances of the model.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the factory that creates the instances of the model.
     * @generated
     */
	Model2Factory getModel2Factory();

	/**
     * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
     * @generated
     */
	interface Literals {
		/**
         * The meta object literal for the '{@link fw2.model2.impl.ActionImpl <em>Action</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ActionImpl
         * @see fw2.model2.impl.Model2PackageImpl#getAction()
         * @generated
         */
		EClass ACTION = eINSTANCE.getAction();

		/**
         * The meta object literal for the '<em><b>Access Key</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute ACTION__ACCESS_KEY = eINSTANCE.getAction_AccessKey();

		/**
         * The meta object literal for the '<em><b>Url</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute ACTION__URL = eINSTANCE.getAction_Url();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.AggregationImpl <em>Aggregation</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.AggregationImpl
         * @see fw2.model2.impl.Model2PackageImpl#getAggregation()
         * @generated
         */
		EClass AGGREGATION = eINSTANCE.getAggregation();

		/**
         * The meta object literal for the '<em><b>Component</b></em>' containment reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference AGGREGATION__COMPONENT = eINSTANCE.getAggregation_Component();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.AnnotationImpl <em>Annotation</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.AnnotationImpl
         * @see fw2.model2.impl.Model2PackageImpl#getAnnotation()
         * @generated
         */
		EClass ANNOTATION = eINSTANCE.getAnnotation();

		/**
         * The meta object literal for the '<em><b>Tags</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference ANNOTATION__TAGS = eINSTANCE.getAnnotation_Tags();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ApplicationImpl <em>Application</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ApplicationImpl
         * @see fw2.model2.impl.Model2PackageImpl#getApplication()
         * @generated
         */
		EClass APPLICATION = eINSTANCE.getApplication();

		/**
         * The meta object literal for the '<em><b>Baps</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference APPLICATION__BAPS = eINSTANCE.getApplication_Baps();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.AssociationImpl <em>Association</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.AssociationImpl
         * @see fw2.model2.impl.Model2PackageImpl#getAssociation()
         * @generated
         */
		EClass ASSOCIATION = eINSTANCE.getAssociation();

		/**
         * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute ASSOCIATION__CARDINALITY = eINSTANCE.getAssociation_Cardinality();

		/**
         * The meta object literal for the '<em><b>Related Class Orm</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference ASSOCIATION__RELATED_CLASS_ORM = eINSTANCE.getAssociation_RelatedClassOrm();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.AttributeImpl <em>Attribute</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.AttributeImpl
         * @see fw2.model2.impl.Model2PackageImpl#getAttribute()
         * @generated
         */
		EClass ATTRIBUTE = eINSTANCE.getAttribute();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.BapImpl <em>Bap</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.BapImpl
         * @see fw2.model2.impl.Model2PackageImpl#getBap()
         * @generated
         */
		EClass BAP = eINSTANCE.getBap();

		/**
         * The meta object literal for the '<em><b>Component Mapping</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference BAP__COMPONENT_MAPPING = eINSTANCE.getBap_ComponentMapping();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ClassOrmImpl <em>Class Orm</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ClassOrmImpl
         * @see fw2.model2.impl.Model2PackageImpl#getClassOrm()
         * @generated
         */
		EClass CLASS_ORM = eINSTANCE.getClassOrm();

		/**
         * The meta object literal for the '<em><b>Aggregations</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference CLASS_ORM__AGGREGATIONS = eINSTANCE.getClassOrm_Aggregations();

		/**
         * The meta object literal for the '<em><b>Data Set</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference CLASS_ORM__DATA_SET = eINSTANCE.getClassOrm_DataSet();

		/**
         * The meta object literal for the '<em><b>Key</b></em>' containment reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference CLASS_ORM__KEY = eINSTANCE.getClassOrm_Key();

		/**
         * The meta object literal for the '<em><b>References</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference CLASS_ORM__REFERENCES = eINSTANCE.getClassOrm_References();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ComponentImpl <em>Component</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ComponentImpl
         * @see fw2.model2.impl.Model2PackageImpl#getComponent()
         * @generated
         */
		EClass COMPONENT = eINSTANCE.getComponent();

		/**
         * The meta object literal for the '<em><b>Lookups</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference COMPONENT__LOOKUPS = eINSTANCE.getComponent_Lookups();

		/**
         * The meta object literal for the '<em><b>Primitives</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference COMPONENT__PRIMITIVES = eINSTANCE.getComponent_Primitives();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ConstantImpl <em>Constant</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ConstantImpl
         * @see fw2.model2.impl.Model2PackageImpl#getConstant()
         * @generated
         */
		EClass CONSTANT = eINSTANCE.getConstant();

		/**
         * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute CONSTANT__TYPE = eINSTANCE.getConstant_Type();

		/**
         * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute CONSTANT__VALUE = eINSTANCE.getConstant_Value();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbColumnImpl <em>Db Column</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbColumnImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbColumn()
         * @generated
         */
		EClass DB_COLUMN = eINSTANCE.getDbColumn();

		/**
         * The meta object literal for the '<em><b>Default</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__DEFAULT = eINSTANCE.getDbColumn_Default();

		/**
         * The meta object literal for the '<em><b>Key Seq</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__KEY_SEQ = eINSTANCE.getDbColumn_KeySeq();

		/**
         * The meta object literal for the '<em><b>Length</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__LENGTH = eINSTANCE.getDbColumn_Length();

		/**
         * The meta object literal for the '<em><b>Nullable</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__NULLABLE = eINSTANCE.getDbColumn_Nullable();

		/**
         * The meta object literal for the '<em><b>Scale</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__SCALE = eINSTANCE.getDbColumn_Scale();

		/**
         * The meta object literal for the '<em><b>Seq</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__SEQ = eINSTANCE.getDbColumn_Seq();

		/**
         * The meta object literal for the '<em><b>Size</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__SIZE = eINSTANCE.getDbColumn_Size();

		/**
         * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN__TYPE = eINSTANCE.getDbColumn_Type();

		/**
         * The meta object literal for the '<em><b>Table</b></em>' container reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_COLUMN__TABLE = eINSTANCE.getDbColumn_Table();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbColumnMapImpl <em>Db Column Map</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbColumnMapImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbColumnMap()
         * @generated
         */
		EClass DB_COLUMN_MAP = eINSTANCE.getDbColumnMap();

		/**
         * The meta object literal for the '<em><b>Db Column Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_COLUMN_MAP__DB_COLUMN_NAME = eINSTANCE.getDbColumnMap_DbColumnName();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbDataSetImpl <em>Db Data Set</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbDataSetImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbDataSet()
         * @generated
         */
		EClass DB_DATA_SET = eINSTANCE.getDbDataSet();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbForeignKeyColumnImpl <em>Db Foreign Key Column</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbForeignKeyColumnImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbForeignKeyColumn()
         * @generated
         */
		EClass DB_FOREIGN_KEY_COLUMN = eINSTANCE.getDbForeignKeyColumn();

		/**
         * The meta object literal for the '<em><b>Key Seq</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_FOREIGN_KEY_COLUMN__KEY_SEQ = eINSTANCE.getDbForeignKeyColumn_KeySeq();

		/**
         * The meta object literal for the '<em><b>Seq</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DB_FOREIGN_KEY_COLUMN__SEQ = eINSTANCE.getDbForeignKeyColumn_Seq();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbJoinKeyImpl <em>Db Join Key</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbJoinKeyImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbJoinKey()
         * @generated
         */
		EClass DB_JOIN_KEY = eINSTANCE.getDbJoinKey();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbJoinParameterImpl <em>Db Join Parameter</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbJoinParameterImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbJoinParameter()
         * @generated
         */
		EClass DB_JOIN_PARAMETER = eINSTANCE.getDbJoinParameter();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbRelationImpl <em>Db Relation</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbRelationImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbRelation()
         * @generated
         */
		EClass DB_RELATION = eINSTANCE.getDbRelation();

		/**
         * The meta object literal for the '<em><b>Foreign Key Columns</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_RELATION__FOREIGN_KEY_COLUMNS = eINSTANCE.getDbRelation_ForeignKeyColumns();

		/**
         * The meta object literal for the '<em><b>Ref Table</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_RELATION__REF_TABLE = eINSTANCE.getDbRelation_RefTable();

		/**
         * The meta object literal for the '<em><b>Table</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_RELATION__TABLE = eINSTANCE.getDbRelation_Table();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbTableImpl <em>Db Table</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbTableImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbTable()
         * @generated
         */
		EClass DB_TABLE = eINSTANCE.getDbTable();

		/**
         * The meta object literal for the '<em><b>Columns</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_TABLE__COLUMNS = eINSTANCE.getDbTable_Columns();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbViewImpl <em>Db View</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbViewImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbView()
         * @generated
         */
		EClass DB_VIEW = eINSTANCE.getDbView();

		/**
         * The meta object literal for the '<em><b>Join Keys</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_VIEW__JOIN_KEYS = eINSTANCE.getDbView_JoinKeys();

		/**
         * The meta object literal for the '<em><b>Join Parameters</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_VIEW__JOIN_PARAMETERS = eINSTANCE.getDbView_JoinParameters();

		/**
         * The meta object literal for the '<em><b>View Columns</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DB_VIEW__VIEW_COLUMNS = eINSTANCE.getDbView_ViewColumns();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DbViewColumnImpl <em>Db View Column</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DbViewColumnImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDbViewColumn()
         * @generated
         */
		EClass DB_VIEW_COLUMN = eINSTANCE.getDbViewColumn();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DiscriminatorKeyImpl <em>Discriminator Key</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DiscriminatorKeyImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDiscriminatorKey()
         * @generated
         */
		EClass DISCRIMINATOR_KEY = eINSTANCE.getDiscriminatorKey();

		/**
         * The meta object literal for the '<em><b>Discriminator Value</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE = eINSTANCE.getDiscriminatorKey_DiscriminatorValue();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DomainAttributeImpl <em>Domain Attribute</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DomainAttributeImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDomainAttribute()
         * @generated
         */
		EClass DOMAIN_ATTRIBUTE = eINSTANCE.getDomainAttribute();

		/**
         * The meta object literal for the '<em><b>Data Type</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DOMAIN_ATTRIBUTE__DATA_TYPE = eINSTANCE.getDomainAttribute_DataType();

		/**
         * The meta object literal for the '<em><b>Multivalued</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DOMAIN_ATTRIBUTE__MULTIVALUED = eINSTANCE.getDomainAttribute_Multivalued();

		/**
         * The meta object literal for the '<em><b>Reference</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DOMAIN_ATTRIBUTE__REFERENCE = eINSTANCE.getDomainAttribute_Reference();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.DomainClassImpl <em>Domain Class</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.DomainClassImpl
         * @see fw2.model2.impl.Model2PackageImpl#getDomainClass()
         * @generated
         */
		EClass DOMAIN_CLASS = eINSTANCE.getDomainClass();

		/**
         * The meta object literal for the '<em><b>Superclass</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DOMAIN_CLASS__SUPERCLASS = eINSTANCE.getDomainClass_Superclass();

		/**
         * The meta object literal for the '<em><b>Platform Class</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DOMAIN_CLASS__PLATFORM_CLASS = eINSTANCE.getDomainClass_PlatformClass();

		/**
         * The meta object literal for the '<em><b>Attributes</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference DOMAIN_CLASS__ATTRIBUTES = eINSTANCE.getDomainClass_Attributes();

		/**
         * The meta object literal for the '<em><b>Abstract</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DOMAIN_CLASS__ABSTRACT = eINSTANCE.getDomainClass_Abstract();

		/**
         * The meta object literal for the '<em><b>Primitive</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute DOMAIN_CLASS__PRIMITIVE = eINSTANCE.getDomainClass_Primitive();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.EditorImpl <em>Editor</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.EditorImpl
         * @see fw2.model2.impl.Model2PackageImpl#getEditor()
         * @generated
         */
		EClass EDITOR = eINSTANCE.getEditor();

		/**
         * The meta object literal for the '<em><b>Editors</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference EDITOR__EDITORS = eINSTANCE.getEditor_Editors();

		/**
         * The meta object literal for the '<em><b>Field Groups</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference EDITOR__FIELD_GROUPS = eINSTANCE.getEditor_FieldGroups();

		/**
         * The meta object literal for the '<em><b>Summary Tables</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference EDITOR__SUMMARY_TABLES = eINSTANCE.getEditor_SummaryTables();

		/**
         * The meta object literal for the '<em><b>Present In Staging</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute EDITOR__PRESENT_IN_STAGING = eINSTANCE.getEditor_PresentInStaging();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.FieldImpl <em>Field</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.FieldImpl
         * @see fw2.model2.impl.Model2PackageImpl#getField()
         * @generated
         */
		EClass FIELD = eINSTANCE.getField();

		/**
         * The meta object literal for the '<em><b>Mandatory</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__MANDATORY = eINSTANCE.getField_Mandatory();

		/**
         * The meta object literal for the '<em><b>Max Length</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__MAX_LENGTH = eINSTANCE.getField_MaxLength();

		/**
         * The meta object literal for the '<em><b>Reference List Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__REFERENCE_LIST_NAME = eINSTANCE.getField_ReferenceListName();

		/**
         * The meta object literal for the '<em><b>Row Index</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__ROW_INDEX = eINSTANCE.getField_RowIndex();

		/**
         * The meta object literal for the '<em><b>Select Disabled</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__SELECT_DISABLED = eINSTANCE.getField_SelectDisabled();

		/**
         * The meta object literal for the '<em><b>Select Header Value</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__SELECT_HEADER_VALUE = eINSTANCE.getField_SelectHeaderValue();

		/**
         * The meta object literal for the '<em><b>Size</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__SIZE = eINSTANCE.getField_Size();

		/**
         * The meta object literal for the '<em><b>Present In Staging</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD__PRESENT_IN_STAGING = eINSTANCE.getField_PresentInStaging();

		/**
         * The meta object literal for the '<em><b>Comments</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute FIELD__COMMENTS = eINSTANCE.getField_Comments();

        /**
         * The meta object literal for the '<em><b>Mapping</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute FIELD__MAPPING = eINSTANCE.getField_Mapping();

        /**
         * The meta object literal for the '{@link fw2.model2.impl.FieldExtensionImpl <em>Field Extension</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.FieldExtensionImpl
         * @see fw2.model2.impl.Model2PackageImpl#getFieldExtension()
         * @generated
         */
		EClass FIELD_EXTENSION = eINSTANCE.getFieldExtension();

		/**
         * The meta object literal for the '<em><b>Extension</b></em>' containment reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference FIELD_EXTENSION__EXTENSION = eINSTANCE.getFieldExtension_Extension();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.FieldGroupImpl <em>Field Group</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.FieldGroupImpl
         * @see fw2.model2.impl.Model2PackageImpl#getFieldGroup()
         * @generated
         */
		EClass FIELD_GROUP = eINSTANCE.getFieldGroup();

		/**
         * The meta object literal for the '<em><b>Actions</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference FIELD_GROUP__ACTIONS = eINSTANCE.getFieldGroup_Actions();

		/**
         * The meta object literal for the '<em><b>Fields</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference FIELD_GROUP__FIELDS = eINSTANCE.getFieldGroup_Fields();

		/**
         * The meta object literal for the '<em><b>Display Order</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD_GROUP__DISPLAY_ORDER = eINSTANCE.getFieldGroup_DisplayOrder();

		/**
         * The meta object literal for the '<em><b>Present In Staging</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute FIELD_GROUP__PRESENT_IN_STAGING = eINSTANCE.getFieldGroup_PresentInStaging();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.FieldGroupExtensionImpl <em>Field Group Extension</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.FieldGroupExtensionImpl
         * @see fw2.model2.impl.Model2PackageImpl#getFieldGroupExtension()
         * @generated
         */
		EClass FIELD_GROUP_EXTENSION = eINSTANCE.getFieldGroupExtension();

		/**
         * The meta object literal for the '<em><b>Extension</b></em>' containment reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference FIELD_GROUP_EXTENSION__EXTENSION = eINSTANCE.getFieldGroupExtension_Extension();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.LookupImpl <em>Lookup</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.LookupImpl
         * @see fw2.model2.impl.Model2PackageImpl#getLookup()
         * @generated
         */
		EClass LOOKUP = eINSTANCE.getLookup();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ModelImpl <em>Model</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ModelImpl
         * @see fw2.model2.impl.Model2PackageImpl#getModel()
         * @generated
         */
		EClass MODEL = eINSTANCE.getModel();

		/**
         * The meta object literal for the '<em><b>Elements</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference MODEL__ELEMENTS = eINSTANCE.getModel_Elements();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ModelElementImpl <em>Model Element</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ModelElementImpl
         * @see fw2.model2.impl.Model2PackageImpl#getModelElement()
         * @generated
         */
		EClass MODEL_ELEMENT = eINSTANCE.getModelElement();

		/**
         * The meta object literal for the '<em><b>Annotations</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference MODEL_ELEMENT__ANNOTATIONS = eINSTANCE.getModelElement_Annotations();

		/**
         * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute MODEL_ELEMENT__NAME = eINSTANCE.getModelElement_Name();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.PrimitiveImpl <em>Primitive</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.PrimitiveImpl
         * @see fw2.model2.impl.Model2PackageImpl#getPrimitive()
         * @generated
         */
		EClass PRIMITIVE = eINSTANCE.getPrimitive();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.PrimitiveExtensionImpl <em>Primitive Extension</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.PrimitiveExtensionImpl
         * @see fw2.model2.impl.Model2PackageImpl#getPrimitiveExtension()
         * @generated
         */
		EClass PRIMITIVE_EXTENSION = eINSTANCE.getPrimitiveExtension();

		/**
         * The meta object literal for the '<em><b>Extension</b></em>' containment reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference PRIMITIVE_EXTENSION__EXTENSION = eINSTANCE.getPrimitiveExtension_Extension();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.PropertyImpl <em>Property</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.PropertyImpl
         * @see fw2.model2.impl.Model2PackageImpl#getProperty()
         * @generated
         */
		EClass PROPERTY = eINSTANCE.getProperty();

		/**
         * The meta object literal for the '<em><b>Value</b></em>' containment reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference PROPERTY__VALUE = eINSTANCE.getProperty_Value();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ReferenceImpl <em>Reference</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ReferenceImpl
         * @see fw2.model2.impl.Model2PackageImpl#getReference()
         * @generated
         */
		EClass REFERENCE = eINSTANCE.getReference();

		/**
         * The meta object literal for the '<em><b>Relation</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference REFERENCE__RELATION = eINSTANCE.getReference_Relation();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.SummaryTableImpl <em>Summary Table</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.SummaryTableImpl
         * @see fw2.model2.impl.Model2PackageImpl#getSummaryTable()
         * @generated
         */
		EClass SUMMARY_TABLE = eINSTANCE.getSummaryTable();

		/**
         * The meta object literal for the '<em><b>Columns</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference SUMMARY_TABLE__COLUMNS = eINSTANCE.getSummaryTable_Columns();

		/**
         * The meta object literal for the '<em><b>Domain Wrapper</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute SUMMARY_TABLE__DOMAIN_WRAPPER = eINSTANCE.getSummaryTable_DomainWrapper();

		/**
         * The meta object literal for the '<em><b>Bap Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute SUMMARY_TABLE__BAP_NAME = eINSTANCE.getSummaryTable_BapName();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.TableColumnImpl <em>Table Column</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.TableColumnImpl
         * @see fw2.model2.impl.Model2PackageImpl#getTableColumn()
         * @generated
         */
		EClass TABLE_COLUMN = eINSTANCE.getTableColumn();

		/**
         * The meta object literal for the '<em><b>Header Value</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TABLE_COLUMN__HEADER_VALUE = eINSTANCE.getTableColumn_HeaderValue();

		/**
         * The meta object literal for the '<em><b>Index</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TABLE_COLUMN__INDEX = eINSTANCE.getTableColumn_Index();

		/**
         * The meta object literal for the '<em><b>Property</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TABLE_COLUMN__PROPERTY = eINSTANCE.getTableColumn_Property();

		/**
         * The meta object literal for the '<em><b>Reference List Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TABLE_COLUMN__REFERENCE_LIST_NAME = eINSTANCE.getTableColumn_ReferenceListName();

		/**
         * The meta object literal for the '<em><b>Select Disabled</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TABLE_COLUMN__SELECT_DISABLED = eINSTANCE.getTableColumn_SelectDisabled();

		/**
         * The meta object literal for the '<em><b>Comments</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute TABLE_COLUMN__COMMENTS = eINSTANCE.getTableColumn_Comments();

        /**
         * The meta object literal for the '<em><b>Mapping</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute TABLE_COLUMN__MAPPING = eINSTANCE.getTableColumn_Mapping();

        /**
         * The meta object literal for the '{@link fw2.model2.impl.TagImpl <em>Tag</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.TagImpl
         * @see fw2.model2.impl.Model2PackageImpl#getTag()
         * @generated
         */
		EClass TAG = eINSTANCE.getTag();

		/**
         * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TAG__TYPE = eINSTANCE.getTag_Type();

		/**
         * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute TAG__VALUE = eINSTANCE.getTag_Value();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.TypeExtensionImpl <em>Type Extension</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.TypeExtensionImpl
         * @see fw2.model2.impl.Model2PackageImpl#getTypeExtension()
         * @generated
         */
		EClass TYPE_EXTENSION = eINSTANCE.getTypeExtension();

		/**
         * The meta object literal for the '<em><b>Domain Class</b></em>' reference feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference TYPE_EXTENSION__DOMAIN_CLASS = eINSTANCE.getTypeExtension_DomainClass();

		/**
         * The meta object literal for the '<em><b>Properties</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference TYPE_EXTENSION__PROPERTIES = eINSTANCE.getTypeExtension_Properties();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ViewComponentImpl <em>View Component</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ViewComponentImpl
         * @see fw2.model2.impl.Model2PackageImpl#getViewComponent()
         * @generated
         */
		EClass VIEW_COMPONENT = eINSTANCE.getViewComponent();

		/**
         * The meta object literal for the '<em><b>Component Id</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute VIEW_COMPONENT__COMPONENT_ID = eINSTANCE.getViewComponent_ComponentId();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ViewElementImpl <em>View Element</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ViewElementImpl
         * @see fw2.model2.impl.Model2PackageImpl#getViewElement()
         * @generated
         */
		EClass VIEW_ELEMENT = eINSTANCE.getViewElement();

		/**
         * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute VIEW_ELEMENT__ID = eINSTANCE.getViewElement_Id();

		/**
         * The meta object literal for the '<em><b>Label</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute VIEW_ELEMENT__LABEL = eINSTANCE.getViewElement_Label();

		/**
         * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute VIEW_ELEMENT__TYPE = eINSTANCE.getViewElement_Type();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ViewPrimitiveImpl <em>View Primitive</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ViewPrimitiveImpl
         * @see fw2.model2.impl.Model2PackageImpl#getViewPrimitive()
         * @generated
         */
		EClass VIEW_PRIMITIVE = eINSTANCE.getViewPrimitive();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.UiFieldTypeImpl <em>Ui Field Type</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.UiFieldTypeImpl
         * @see fw2.model2.impl.Model2PackageImpl#getUiFieldType()
         * @generated
         */
		EClass UI_FIELD_TYPE = eINSTANCE.getUiFieldType();

		/**
         * The meta object literal for the '<em><b>Data Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute UI_FIELD_TYPE__DATA_TYPE = eINSTANCE.getUiFieldType_DataType();

		/**
         * The meta object literal for the '<em><b>Html Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute UI_FIELD_TYPE__HTML_TYPE = eINSTANCE.getUiFieldType_HtmlType();

		/**
         * The meta object literal for the '<em><b>On Screen Validation Method</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD = eINSTANCE.getUiFieldType_OnScreenValidationMethod();

		/**
         * The meta object literal for the '<em><b>Properties</b></em>' containment reference list feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EReference UI_FIELD_TYPE__PROPERTIES = eINSTANCE.getUiFieldType_Properties();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.UiFieldTypePropertyImpl <em>Ui Field Type Property</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.UiFieldTypePropertyImpl
         * @see fw2.model2.impl.Model2PackageImpl#getUiFieldTypeProperty()
         * @generated
         */
		EClass UI_FIELD_TYPE_PROPERTY = eINSTANCE.getUiFieldTypeProperty();

		/**
         * The meta object literal for the '<em><b>Property Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME = eINSTANCE.getUiFieldTypeProperty_PropertyName();

		/**
         * The meta object literal for the '<em><b>Property Type</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE = eINSTANCE.getUiFieldTypeProperty_PropertyType();

		/**
         * The meta object literal for the '<em><b>Type Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute UI_FIELD_TYPE_PROPERTY__TYPE_NAME = eINSTANCE.getUiFieldTypeProperty_TypeName();

		/**
         * The meta object literal for the '{@link fw2.model2.impl.ComponentMappingImpl <em>Component Mapping</em>}' class.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.impl.ComponentMappingImpl
         * @see fw2.model2.impl.Model2PackageImpl#getComponentMapping()
         * @generated
         */
		EClass COMPONENT_MAPPING = eINSTANCE.getComponentMapping();

		/**
         * The meta object literal for the '<em><b>Class Name</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute COMPONENT_MAPPING__CLASS_NAME = eINSTANCE.getComponentMapping_ClassName();

		/**
         * The meta object literal for the '<em><b>Component Id</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute COMPONENT_MAPPING__COMPONENT_ID = eINSTANCE.getComponentMapping_ComponentId();

		/**
         * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute COMPONENT_MAPPING__ID = eINSTANCE.getComponentMapping_Id();

		/**
         * The meta object literal for the '<em><b>Type Code</b></em>' attribute feature.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @generated
         */
		EAttribute COMPONENT_MAPPING__TYPE_CODE = eINSTANCE.getComponentMapping_TypeCode();

		/**
         * The meta object literal for the '{@link fw2.model2.CardinalityTypeEnum <em>Cardinality Type Enum</em>}' enum.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.CardinalityTypeEnum
         * @see fw2.model2.impl.Model2PackageImpl#getCardinalityTypeEnum()
         * @generated
         */
		EEnum CARDINALITY_TYPE_ENUM = eINSTANCE.getCardinalityTypeEnum();

		/**
         * The meta object literal for the '{@link fw2.model2.DbColumnTypeEnum <em>Db Column Type Enum</em>}' enum.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.DbColumnTypeEnum
         * @see fw2.model2.impl.Model2PackageImpl#getDbColumnTypeEnum()
         * @generated
         */
		EEnum DB_COLUMN_TYPE_ENUM = eINSTANCE.getDbColumnTypeEnum();

		/**
         * The meta object literal for the '{@link fw2.model2.FieldTypeEnum <em>Field Type Enum</em>}' enum.
         * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
         * @see fw2.model2.FieldTypeEnum
         * @see fw2.model2.impl.Model2PackageImpl#getFieldTypeEnum()
         * @generated
         */
		EEnum FIELD_TYPE_ENUM = eINSTANCE.getFieldTypeEnum();

	}

} //Model2Package
