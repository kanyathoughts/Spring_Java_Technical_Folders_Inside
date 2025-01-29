/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.lang.StringUtils;


@XmlJavaTypeAdapter(UtilityOutbound.Adapter.class)
public class UtilityOutbound implements Serializable {
	private static final Type DEFAULT_PROGRAM = Type.UNKNOWN;
	private static final String DEFAULT_GROUP = "0";
	private static final Technology DEFAULT_TECHNOLOGY = Technology.UNKNOWN;
	
	final Technology technology;
	final Type type;
	final String group;
	final String outboundValue;
	
	public UtilityOutbound(final Technology technology, final Type type, final String group, final String outboundValue) {
		this.technology = technology;
		this.type = type;
		this.group = group;
		this.outboundValue = outboundValue;
	}

	public Technology getTechnology() {
		return technology;
	}

	public Type getType() {
		return type;
	}
	
	public String getGroup() {
		return group;
	}
	
	public String getOutBoundValue() {
		return outboundValue;
	}
	
	static class Adapter extends XmlAdapter<UtilityOutBoundAdapted, UtilityOutbound> {

		@Override
		public UtilityOutbound unmarshal(@Nullable final UtilityOutBoundAdapted adapted) throws Exception {
			if (adapted != null) {
				final Technology technology = adapted.technology != null ? adapted.technology : DEFAULT_TECHNOLOGY;
				final Type type = adapted.type != null ? adapted.type : DEFAULT_PROGRAM;
				final String group = adapted.group != null ? adapted.group : DEFAULT_GROUP;
				final String outboundValue = adapted.outboundValue != null ? adapted.outboundValue : StringUtils.EMPTY;
				return new UtilityOutbound(technology, type, group, outboundValue);
			}
			return null;
		}

		@Override
		public UtilityOutBoundAdapted marshal(@Nullable final UtilityOutbound entity) throws Exception {
			if (entity != null) {
				final UtilityOutBoundAdapted result = new UtilityOutBoundAdapted();
				result.technology = entity.technology == DEFAULT_TECHNOLOGY ? null : entity.technology;
				result.type = entity.type == DEFAULT_PROGRAM ? null : entity.type;
				result.group = entity.group.equals(DEFAULT_GROUP) ? null: entity.group;
				result.outboundValue = entity.outboundValue.equals(StringUtils.EMPTY) ? null : entity.outboundValue;
				return result;
			}
			return null;
		}
	}
	
	@XmlRootElement(name="outbound")
	private static class UtilityOutBoundAdapted {
		@XmlAttribute
		@Nullable 
		private Technology technology;
		
		@XmlAttribute
		@Nullable 
		private Type type;
		
		@XmlAttribute
		@Nullable 
		private String group;
		
		@XmlValue
		@Nullable 
		private String outboundValue;
	}	

}
