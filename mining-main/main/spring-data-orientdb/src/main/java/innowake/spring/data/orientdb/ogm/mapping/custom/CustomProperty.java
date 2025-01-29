/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.custom;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang.builder.ToStringBuilder;
import com.google.gson.Gson;
import com.orientechnologies.orient.core.db.record.OIdentifiable;
import com.orientechnologies.orient.core.id.ORID;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.CustomPropertyDataType;

/**
 * A custom database property, which is represented by its name, type and value.
 */
public class CustomProperty {

	private final Gson gson = new Gson();
	private final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
	private final DateFormat dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	
	@Nullable
	private String name;
	@Nullable
	private String value;
	@Nullable
	private CustomPropertyDataType dataType;
	
	/**
	 * Default constructor used by proxy to create an instance.
	 */
	public CustomProperty() {
		
	}

	/**
	 * Instantiates {@link CustomProperty} object.
	 * 
	 * @param name the name of the property
	 * @param data the value of the property
	 * @param dataType type of value
	 */
	public CustomProperty(final String name, final @Nullable Object data, final CustomPropertyDataType dataType) {
		this.name = name;
		this.value = data == null ? null : getValueAsString(data, dataType);
		this.dataType = dataType;
	}

	/**
	 * Returns the name.
	 *
	 * @return the name
	 */
	@Nullable
	public String getName() {
		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Returns the value.
	 *
	 * @return the value
	 */
	@Nullable
	public String getValue() {
		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the value to set
	 */
	public void setValue(@Nullable final String value) {
		this.value = value;
	}

	/**
	 * Returns the dataType.
	 *
	 * @return the dataType
	 */
	@Nullable
	public CustomPropertyDataType getDataType() {
		return dataType;
	}

	/**
	 * Sets the dataType.
	 *
	 * @param dataType the dataType to set
	 */
	public void setDataType(final CustomPropertyDataType dataType) {
		this.dataType = dataType;
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("name", name);
		builder.append("value", value);
		builder.append("dataType", dataType);
		return builder.toString();
	}
	
	private String getValueAsString(final Object object, final CustomPropertyDataType type) {
		if (object instanceof Date) {
			if (type == CustomPropertyDataType.DATE) {
				return dateFormat.format((Date) object);
			} else if (type == CustomPropertyDataType.DATETIME) {
				return dateTimeFormat.format((Date) object);
			} 
		}

		if (type != CustomPropertyDataType.EMBEDDEDMAP && type != CustomPropertyDataType.EMBEDDEDLIST && type != CustomPropertyDataType.LINKLIST) {
			return object.toString();
		}
		
		if ( ! (object instanceof List) && ! (object instanceof Map)) {
			final String message = String.format("Unsupported class %s for custom property of type %s", object.getClass().getSimpleName(), type.toString());
			throw new UnsupportedOperationException(message);
		}
		
		if (type == CustomPropertyDataType.LINKLIST) {
			/* convert LINKLIST to list of RIDs as string */
			final List<String> result = new ArrayList<>();
			for (final Object identifyable : (List<?>) object) {
				if (identifyable instanceof String) {
					/* is already string, so nothing to do */
					result.add((String) identifyable);
				} else if (identifyable instanceof OIdentifiable) {
					final ORID identity = ((OIdentifiable) identifyable).getIdentity();
					result.add(identity.toString());
				}
			}
			
			/* return list in JSON format */
			return gson.toJson(result);
		}

		/* return in JSON format */
		return gson.toJson(object);
	}
	
}
