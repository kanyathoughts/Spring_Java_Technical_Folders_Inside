/**
 */
package fw2.model2.util;

import fw2.model2.Model2Package;

import java.util.Map;

import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageRegistryImpl;

import org.eclipse.emf.ecore.resource.Resource;

import org.eclipse.emf.ecore.xmi.util.XMLProcessor;

/**
 * This class contains helper methods to serialize and deserialize XML documents
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class Model2XMLProcessor extends XMLProcessor {

	/**
     * Public constructor to instantiate the helper.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model2XMLProcessor() {
        super(new EPackageRegistryImpl(EPackage.Registry.INSTANCE));
        extendedMetaData.putPackage(null, Model2Package.eINSTANCE);
    }
	
	/**
     * Register for "*" and "xml" file extensions the Model2ResourceFactoryImpl factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected Map<String, Resource.Factory> getRegistrations() {
        if (registrations == null) {
            super.getRegistrations();
            registrations.put(XML_EXTENSION, new Model2ResourceFactoryImpl());
            registrations.put(STAR_EXTENSION, new Model2ResourceFactoryImpl());
        }
        return registrations;
    }

} //Model2XMLProcessor
