/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.core;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.model.discovery.ResolveTarget;
/**
 * Represents one property settings for a language.
 */
@XmlJavaTypeAdapter(Property.Adapter.class)
public class Property implements Serializable {
	private final ResolveTarget language;
	private final Map<String, List<String>> settings;
	
	public Property(final ResolveTarget language, final KeyValuePair... settings) {
		this.language = language;
		this.settings = Arrays.asList(settings).stream()
				.collect(Collectors.groupingBy(
						KeyValuePair::getKey, 
						LinkedHashMap::new,
						Collectors.mapping(KeyValuePair::getValue, Collectors.toList())));
	}
	
	public Property(final ResolveTarget language, final List<KeyValuePair> settings) {
		this.language = language;
		this.settings = settings.stream()
				.collect(Collectors.groupingBy(
						KeyValuePair::getKey, 
						LinkedHashMap::new,
						Collectors.mapping(KeyValuePair::getValue, Collectors.toList())));
	}

	public ResolveTarget getLanguage() {
		return language;
	}

	public Map<String, List<String>> getSettings() {
		return settings;
	}
	
	static class Adapter extends XmlAdapter<PropertyAdapted, Property> {

		@Override
		public Property unmarshal(@Nullable final PropertyAdapted adapted) throws Exception {
			final PropertyAdapted adaptedNN = assertNotNull(adapted);	
			return new Property(
				assertNotNull(adaptedNN.language).getTarget(),
				assertNotNull(adaptedNN.settings)
			);
		}

		@Override
		public PropertyAdapted marshal(@Nullable final Property pattern) throws Exception {
			final Property patternNN = assertNotNull(pattern);
			final PropertyAdapted result = new PropertyAdapted();
			result.language = new XMLResolveTarget(patternNN.language);
			result.settings = patternNN.settings.entrySet().stream()
					.flatMap(s -> s.getValue().stream().map(e -> new KeyValuePair(s.getKey(), e)))
					.collect(Collectors.toList());
			return result;
		}
		
	}
	
	@XmlRootElement(name="property")
	private static class PropertyAdapted {
		
		@XmlAttribute
		@Nullable
		private XMLResolveTarget language;
		
		@XmlElement(name="setting")
		@Nullable
		private List<KeyValuePair> settings;
		
		
	}

}
