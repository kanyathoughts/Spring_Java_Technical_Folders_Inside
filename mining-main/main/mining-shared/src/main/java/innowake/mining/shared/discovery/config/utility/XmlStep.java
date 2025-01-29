/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static java.util.Arrays.stream;
import java.io.Serializable;
import java.util.Optional;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.Logging;

/**
 * This represents a step tag in the utilities.xml file with attributes.
 */
@XmlJavaTypeAdapter(XmlStep.Adapter.class)
public class XmlStep implements Serializable {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONFIG);
	
	final String name;
	
	final AccessType accessType;
	
	/**
	 * Creates a new {@link XmlStep}.
	 * 
	 * @param name the value of the step
	 * @param accessType the attribute of the step
	 */
	public XmlStep(final String name, final AccessType accessType) {
		this.name = name;
		this.accessType = accessType;
	}

	/**
	 * Gets the value of the step tag.
	 *
	 * @return the the value of the step tag
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Gets the attribute {@code accessType} value of the step tag.
	 *
	 * @return the accessType attribute value of step tag
	 */
	public AccessType getAccessType() {
		return accessType;
	}
	
	static class Adapter extends XmlAdapter<XmlStepAdapted, XmlStep> {

		@Override
		public XmlStep unmarshal(@Nullable final XmlStepAdapted adapted) throws Exception {
			final XmlStepAdapted adaptedNN = assertNotNull(adapted);	
			
			final String name;
			final AccessType accessType;
			
			name = adaptedNN.name != null ? adaptedNN.name : "";
			accessType = adaptedNN.accessType != null ? getQualifiedAccessType(adaptedNN) : null;
					
			return new XmlStep(name, accessType);
		}

		@Override
		public XmlStepAdapted marshal(@Nullable final XmlStep entity) throws Exception {
			final XmlStep entityNN = assertNotNull(entity);
			final XmlStepAdapted result = new XmlStepAdapted();
			result.name = entityNN.name.isEmpty() ? null: entityNN.name;
			result.accessType = entityNN.accessType != null ? entityNN.accessType.name() : null;
			
			return result;
		}
		
		private AccessType getQualifiedAccessType(final XmlStepAdapted adapted) {
			final Optional<AccessType> accessType = stream(AccessType.values())
					.filter(field -> field.name().equalsIgnoreCase(adapted.accessType))
					.findAny();
			if (accessType.isPresent()) {
				return accessType.get();
			} else {
				LOG.error(String.format("Invalid access type %s provided with the step %s.", adapted.accessType, adapted.name));
				throw new EnumConstantNotPresentException(AccessType.class, adapted.accessType);
			}
		}
	}
	
	@XmlRootElement(name="step")
	private static class XmlStepAdapted {
		
		@XmlValue
		@Nullable 
		private String name;
		
		@XmlAttribute(name="accessType")
		@Nullable 
		private String accessType;
	}
	
	/**
	 * This represents an attribute of access type in step tag.
	 */
	public enum AccessType {
		READ, WRITE
	}
}
