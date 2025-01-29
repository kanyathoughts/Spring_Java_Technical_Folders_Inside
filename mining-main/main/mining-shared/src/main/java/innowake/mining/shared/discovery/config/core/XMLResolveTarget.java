/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.core;

import static java.util.Arrays.stream;

import java.util.Locale;
import java.util.Optional;

import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang.WordUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Wrapper for resolve target used for serialization of xml
 */
@XmlJavaTypeAdapter(XMLResolveTarget.Adapter.class)
public class XMLResolveTarget {

	private ResolveTarget target = ResolveTarget.NONE;
	public ResolveTarget getTarget() { return target; }

	public XMLResolveTarget( final ResolveTarget target ) {
		this.target = target;
	}
	
	/**
	 * Resolves an element by its qualified id.
	 * <p>
	 * <b>Example:</b>
	 * <pre>
	 * COBOL_PROGRAM -> COBOL/PROGRAM   
	 * JCL_INLINE_PROC -> JCL/INLINE_PROC     
	 * </pre>
	 *
	 * @param qualifiedId the qualified id
	 * @return the resolved element
	 * @see #getQualifiedId()
	 */
	public static Optional<ResolveTarget> forQualifiedId(final String qualifiedId) {
		return stream(ResolveTarget.values())
				.filter(candidate -> new XMLResolveTarget(candidate).getQualifiedId().equalsIgnoreCase(qualifiedId))
				.findAny();
	}
	
	/**
	 * Returns the qualified id of this element starting with the id of its root, each element separated by '/'.
	 * <p>
	 * <b>Example:</b>
	 * <pre>
	 * COBOL_PROGRAM -> COBOL/PROGRAM   
	 * JCL_INLINE_PROC -> JCL/INLINE_PROC       
	 * </pre>
	 *
	 * @return the qualified id of this element starting from its root
	 */
	public String getQualifiedId() {
		return this.target.name().replaceFirst("_", "/");
	}
	
	/**
	 * Returns the formatted name of this element.
	 * Ex. COBOL_PROGRAM will become Cobol Program
	 *
	 * @return the formatted name of this element
	 */
	public String getNameFormatted() {
		return WordUtils.capitalize(this.target.name().toLowerCase(Locale.ENGLISH).replace("_", " "));
	}

	/**
	 * Adapts for custom marshaling.
	 * @see XmlAdapter
	 */
	public static class Adapter extends XmlAdapter<String, XMLResolveTarget> {

		@Override
		public XMLResolveTarget unmarshal(final @Nullable String name) throws Exception {
			if( name == null ) {
				throw new IllegalArgumentException("Error unmarshalling to resolve target, name is null");
			}
			return new XMLResolveTarget(XMLResolveTarget.forQualifiedId(name)
								.orElseThrow(() -> new EnumConstantNotPresentException(ResolveTarget.class, name) ));
		}

		@Override
		public String marshal(final @Nullable XMLResolveTarget target) throws Exception {
			if( target == null ) {
				throw new IllegalArgumentException("Error marshalling resolve target, target is null");
			}
			return target.getQualifiedId();
		}

	}

}
