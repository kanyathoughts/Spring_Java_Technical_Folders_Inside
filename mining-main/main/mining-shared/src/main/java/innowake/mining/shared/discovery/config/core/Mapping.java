/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.core;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Represents one pattern mapping entry in the config file.
 */
@XmlJavaTypeAdapter(Mapping.MappingAdapter.class)
public class Mapping implements Serializable {
	
	public enum MappingType {SOLID, MAYBE}
	
	public final String pattern;
	public final ResolveTarget type;
	public final String folder;
	
	public Mapping(final String pattern, final ResolveTarget type, final String folder) {
		this.pattern = pattern;
		this.type = type;
		this.folder = folder;
	}

	@Override
	public String toString() {
		return String.format("pattern='%s', type='%s', folder='%s'", pattern, type, folder);
	}
	
	public static class MappingAdapter extends XmlAdapter<MappingAdapted, Mapping> {

		@Override
		public Mapping unmarshal(@Nullable final MappingAdapted adapted) throws Exception {
			final MappingAdapted adaptedNN = assertNotNull(adapted);
			
			return new Mapping(
					assertNotNull(adaptedNN.pattern),
					assertNotNull(adaptedNN.type).getTarget(),
					assertNotNull(adaptedNN.folder)
			);
		}

		@Override
		public MappingAdapted marshal(@Nullable final Mapping pattern) throws Exception {
			final Mapping patternNN = assertNotNull(pattern);
			
			final MappingAdapted result = new MappingAdapted();
			result.pattern = patternNN.pattern;
			result.type = new XMLResolveTarget(patternNN.type);
			result.folder = patternNN.folder;
			return result;
		}
		
	}
	
	@XmlRootElement(name="mapping")
	private static class MappingAdapted {
		
		@XmlAttribute
		@Nullable
		private String pattern;
		
		@XmlAttribute
		@Nullable
		private XMLResolveTarget type;
		
		@XmlAttribute
		@Nullable
		private String folder;
		
	}
}
