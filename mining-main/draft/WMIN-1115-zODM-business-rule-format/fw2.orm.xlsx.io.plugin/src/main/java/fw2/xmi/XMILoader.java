package fw2.xmi;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;

public class XMILoader<T extends EObject> {

	public XMILoader() {
		super();
	}

	@SuppressWarnings("unchecked")
	public T load(final String uri) {
	
		final Resource.Factory.Registry reg = Resource.Factory.Registry.INSTANCE;
		final Map<String, Object> m = reg.getExtensionToFactoryMap();
		m.put("xmi", new XMIResourceFactoryImpl());
	
		final ResourceSet resSet = new ResourceSetImpl();
		final Resource resource = resSet.getResource(URI.createURI(uri), true);
		final T object = (T) resource.getContents().get(0);
	
		return object;
	}
	public T load(final URI uri) {
		
		final Resource.Factory.Registry reg = Resource.Factory.Registry.INSTANCE;
		final Map<String, Object> m = reg.getExtensionToFactoryMap();
		m.put("xmi", new XMIResourceFactoryImpl());
	
		final ResourceSet resSet = new ResourceSetImpl();
		final Resource resource = resSet.getResource(uri, true);
		@SuppressWarnings("unchecked")
		final T object = (T) resource.getContents().get(0);
	
		return object;
	}
	
	public void save(T object, final URI uri) {
		
		final ResourceSet resourceSet = new ResourceSetImpl();
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());
		final Resource resource = resourceSet.createResource(uri);	
		resource.getContents().add(object);
		final Map<String, Boolean> options = new HashMap<String, Boolean>();
		options.put(XMLResource.OPTION_SCHEMA_LOCATION, Boolean.TRUE);
		try {
			resource.save(options);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	public void save(T object, final String uri) {
		
		final ResourceSet resourceSet = new ResourceSetImpl();
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());
		final Resource resource = resourceSet.createResource(URI.createURI(uri));	
		resource.getContents().add(object);
		final Map<String, Boolean> options = new HashMap<String, Boolean>();
		options.put(XMLResource.OPTION_SCHEMA_LOCATION, Boolean.TRUE);
		try {
			resource.save(options);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}