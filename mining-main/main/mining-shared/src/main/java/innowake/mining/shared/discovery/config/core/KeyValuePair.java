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
import innowake.mining.shared.discovery.config.core.KeyValuePair;

/**
 * Represents one key and value pair in the config file.
 */

@XmlJavaTypeAdapter(KeyValuePair.Adapter.class)
public class KeyValuePair implements Serializable {
	private final String key;
	private final String value;
	
	public KeyValuePair(final String key, final String value) {
		this.key = key;
		this.value = value;
	}
	
	String getKey() {
		return key;
	}

	String getValue() {
		return value;
	}
	
	static class Adapter extends XmlAdapter<KeyValuePairAdapted, KeyValuePair> {

		@Override
		public KeyValuePair unmarshal(@Nullable final KeyValuePairAdapted adapted) throws Exception {
			final KeyValuePairAdapted adaptedNN = assertNotNull(adapted);	
			return new KeyValuePair(
				assertNotNull(adaptedNN.key),
				assertNotNull(adaptedNN.value)
			);
		}

		@Override
		public KeyValuePairAdapted marshal(@Nullable final KeyValuePair pattern) throws Exception {
			final KeyValuePair patternNN = assertNotNull(pattern);
			final KeyValuePairAdapted result = new KeyValuePairAdapted();
			result.key = patternNN.key;
			result.value = patternNN.value;
			return result;
		}
		
	}
	
	@XmlRootElement(name="setting")
	private static class KeyValuePairAdapted {
		
		@XmlAttribute
		@Nullable
		private String key;
		
		@XmlAttribute
		@Nullable
		private String value;
		
	}	

}
