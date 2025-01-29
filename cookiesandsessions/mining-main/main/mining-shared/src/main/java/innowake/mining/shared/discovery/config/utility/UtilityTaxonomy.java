/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import innowake.lib.core.api.lang.Nullable;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;

/**
 * Model class for a utility taxonomy entity.
 */
@XmlJavaTypeAdapter(UtilityTaxonomy.Adapter.class)
public class UtilityTaxonomy implements Serializable {

	final String type;
	final String taxonomy;

	public UtilityTaxonomy(final String type, final String taxonomy) {
		this.type = type;
		this.taxonomy = taxonomy;
	}

	public String getTaxonomy() {
		return taxonomy;
	}

	public String getType() {
		return type;
	}

	static class Adapter extends XmlAdapter<UtilityTaxonomyAdapted, UtilityTaxonomy> {

		@Override
		public UtilityTaxonomy unmarshal(@Nullable final UtilityTaxonomyAdapted adapted) throws Exception {
			if (adapted != null) {
				final String type = adapted.type != null ? adapted.type : "";
				final String taxonomy = adapted.taxonomy != null ? adapted.taxonomy : "";
				return new UtilityTaxonomy(type, taxonomy);
			}
			return null;
		}

		@Override
		public UtilityTaxonomyAdapted marshal(@Nullable final UtilityTaxonomy entity) throws Exception {
			if (entity != null) {
				final UtilityTaxonomyAdapted result = new UtilityTaxonomyAdapted();
				result.taxonomy = entity.taxonomy.isEmpty() ? null : entity.taxonomy;
				result.type = entity.type.isEmpty() ? null : entity.type;
				return result;
			}
			return null;
		}
	}

	@XmlRootElement(name="taxonomy")
	private static class UtilityTaxonomyAdapted {

		@XmlAttribute
		@Nullable
		private String type;

		@XmlValue
		@Nullable
		private String taxonomy;
	}
}
