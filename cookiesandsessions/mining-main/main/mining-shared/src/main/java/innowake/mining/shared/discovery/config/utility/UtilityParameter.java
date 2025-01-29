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

@XmlJavaTypeAdapter(UtilityParameter.Adapter.class)
public class UtilityParameter implements Serializable {
	final String inbound;
	final UtilityOutbound outbound;
	
	public UtilityParameter(final String inbound, final UtilityOutbound outbound) {
		this.inbound = inbound;
		this.outbound = outbound;
	}

	public String getInbound() {
		return inbound;
	}

	public UtilityOutbound getOutbound() {
		return outbound;
	}
	
	static class Adapter extends XmlAdapter<UtilityParameterAdapted, UtilityParameter> {

		@Override
		public UtilityParameter unmarshal(@Nullable final UtilityParameterAdapted adapted) throws Exception {
			final UtilityParameterAdapted adaptedNN = assertNotNull(adapted);	
			
			final String inbound;
			final UtilityOutbound outbound;
			
			inbound = adaptedNN.inbound;
			outbound = adaptedNN.outbound;
			
			return new UtilityParameter(inbound, outbound);
		}

		@Override
		public UtilityParameterAdapted marshal(@Nullable final UtilityParameter entity) throws Exception {
			final UtilityParameter entityNN = assertNotNull(entity);
			final UtilityParameterAdapted result = new UtilityParameterAdapted();
			result.inbound = entityNN.inbound;
			result.outbound = entityNN.outbound;
			return result;
		}
	}
	
	@XmlRootElement(name="parameter")
	private static class UtilityParameterAdapted {
		@XmlElement(name="inbound")
		@Nullable 
		private String inbound;
		
		@XmlElement(name="outbound")
		@Nullable 
		private UtilityOutbound outbound;
		
		
	}	
}
