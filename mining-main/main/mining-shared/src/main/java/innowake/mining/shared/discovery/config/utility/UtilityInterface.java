/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;



@XmlJavaTypeAdapter(UtilityInterface.Adapter.class)
public class UtilityInterface implements Serializable {
	final String name;
	
	public UtilityInterface(final String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	
	static class Adapter extends XmlAdapter<UtilityInterfaceAdapted, UtilityInterface> {

		@Override
		public UtilityInterface unmarshal(@Nullable final UtilityInterfaceAdapted adapted) throws Exception {
			final UtilityInterfaceAdapted adaptedNN = assertNotNull(adapted);	
			
			final String name;
			name = adaptedNN.name;
			return new UtilityInterface(name);
		}

		@Override
		public UtilityInterfaceAdapted marshal(@Nullable final UtilityInterface entity) throws Exception {
			if (entity != null) {
				final UtilityInterfaceAdapted result = new UtilityInterfaceAdapted();
				result.name = entity.name;
				return result;
			}
			return null;
		}
	}
	
	@XmlRootElement(name="interface")
	private static class UtilityInterfaceAdapted {
		@XmlElement(name="interfaceName")
		@Nullable 
		private String name;
	}	
}
