/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.model.discovery.ResolveTarget;

@XmlJavaTypeAdapter(UtilityEntity.Adapter.class)
public class UtilityEntity implements Serializable {

	private final String comments;
	private final String description;
	private final String infoOrigin;
	private final UtilityInterface utilityInterface;
	private final List<UtilityParameter> parameters;
	private final List<XmlStep> steps;
	private final String moduleName;
	private final String packageName;
	private final String supported;
	private final List<UtilityTaxonomy> taxonomies;
	private final List<ResolveTarget> invokingLanguages;
	private final List<String> categories;
	
	public UtilityEntity(final UtilityEntity utilityEntity) {
		this.comments = utilityEntity.comments;
		this.description = utilityEntity.description;
		this.utilityInterface = utilityEntity.utilityInterface;
		this.parameters = utilityEntity.parameters;
		this.steps = utilityEntity.steps;
		this.infoOrigin = utilityEntity.infoOrigin;
		this.moduleName = utilityEntity.moduleName;
		this.packageName = utilityEntity.packageName;
		this.supported = utilityEntity.supported;
		this.taxonomies = utilityEntity.taxonomies;
		this.invokingLanguages = utilityEntity.invokingLanguages;
		this.categories = utilityEntity.categories ;
	}

	public UtilityEntity(final String comments, final String description,
			final String infoOrigin, 
			final UtilityInterface utilityInterface,
			final List<UtilityParameter> parameters, 
			final List<XmlStep> steps,
			final String moduleName, 
			final String packageName,
			final String supported, 
			final List<UtilityTaxonomy> taxonomies,
			final List<ResolveTarget> invokingLanguages,
			final List<String> categories ) {

		this.comments = comments;
		this.description = description;
		this.utilityInterface = utilityInterface;
		this.parameters = parameters;
		this.steps = steps;
		this.infoOrigin = infoOrigin;
		this.moduleName = moduleName;
		this.packageName = packageName;
		this.supported = supported;
		this.taxonomies = taxonomies;
		this.invokingLanguages = invokingLanguages;
		this.categories = categories ;
	}
	
	public String getComments() {
		return comments;
	}

	
	public String getDescription() {
		return description;
	}

	
	public String getInfoOrigin() {
		return infoOrigin;
	}
	
	public boolean isInterface(){
		return utilityInterface != null;
	}
	

	public UtilityInterface getUtilityInterface() {
		return utilityInterface;
	}

	public String getModuleName() {
		return moduleName;
	}

	
	public String getPackageName() {
		return packageName;
	}

	
	public String getSupported() {
		return supported;
	}

	
	public List<UtilityTaxonomy> getTaxonomies() {
		return taxonomies;
	}

	public List<XmlStep> getSteps() {
		return steps;
	}
	
	public List<UtilityParameter> getParameters() {
		return parameters;
	}

	public List<ResolveTarget> getInvokingLanguages() {
		return invokingLanguages;
	}
	
	public List<String> getCategories() {
		return categories;
	}
	
	static class Adapter extends XmlAdapter<UtilityEntityAdapted, UtilityEntity> {

		@Override
		public UtilityEntity unmarshal(@Nullable final UtilityEntityAdapted adapted) throws Exception {
			final UtilityEntityAdapted adaptedNN = assertNotNull(adapted);	
			
			final String comments;
			final String description;
			final String infoOrigin;
			final UtilityInterface utilityInterface;
			final List<UtilityParameter> parameters;
			final List<XmlStep> steps;
			final String moduleName;
			final String packageName;
			final String supported;
			final List<UtilityTaxonomy> taxonomies;
			final List<ResolveTarget> invokingLanguages;
			final List<String> categories;
			
			comments = adaptedNN.comments != null ? adaptedNN.comments : "";
			description = adaptedNN.description != null ? adaptedNN.description : "";
			infoOrigin = adaptedNN.infoOrigin != null ? adaptedNN.infoOrigin : "";
			utilityInterface = adaptedNN.utilityInterface;
			parameters = adaptedNN.parameters != null ? adaptedNN.parameters : Collections.emptyList();
			steps = adaptedNN.steps != null ? adaptedNN.steps : Collections.emptyList();
			moduleName = adaptedNN.moduleName != null ? adaptedNN.moduleName : "";
			packageName = adaptedNN.packageName != null ? adaptedNN.packageName : "";
			supported = adaptedNN.supported != null ? adaptedNN.supported : "";
			taxonomies = adaptedNN.taxonomies != null ? adaptedNN.taxonomies : Collections.emptyList();
			invokingLanguages = adaptedNN.invokingLanguages != null ? adaptedNN.invokingLanguages : Collections.emptyList();	
			categories = adaptedNN.categories != null ? adaptedNN.categories : Collections.emptyList();
			
			return new UtilityEntity(comments, description, infoOrigin, utilityInterface, parameters, steps, moduleName, packageName,
					 supported, taxonomies, invokingLanguages, categories);
		}

		@Override
		public UtilityEntityAdapted marshal(@Nullable final UtilityEntity entity) throws Exception {
			final UtilityEntity entityNN = assertNotNull(entity);
			final UtilityEntityAdapted result = new UtilityEntityAdapted();
			result.comments = entityNN.comments;
			result.description = entityNN.description;
			result.infoOrigin = entityNN.infoOrigin;
			result.utilityInterface = entityNN.utilityInterface;
			result.parameters = entityNN.parameters;
			result.steps = entityNN.steps;
			result.moduleName = entityNN.moduleName;
			result.packageName = entityNN.packageName;
			result.supported = entityNN.supported;
			result.taxonomies = entityNN.taxonomies;
			result.invokingLanguages = entityNN.invokingLanguages;
			result.categories = entityNN.categories;
			return result;
		}
	}
	
	@XmlRootElement(name="Entity")
	private static class UtilityEntityAdapted {

		@XmlAttribute(namespace = XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI)
		@Nullable
		private String type = "utility";

		@XmlElement(name="moduleName")
		@Nullable
		private String moduleName;

		@XmlElement(name="prefix")
		@Nullable
		private String prefix;

		@XmlElementWrapper(name="taxonomies")
		@XmlElement(name="taxonomy")
		@Nullable
		private List<UtilityTaxonomy> taxonomies;

		@XmlElement(name="packageName")
		@Nullable
		private String packageName;

		@XmlElement(name="description")
		@Nullable
		private String description;

		@XmlElement(name="infoOrigin")
		@Nullable
		private String infoOrigin;

		@XmlElement(name="supported")
		@Nullable
		private String supported;

		@XmlElement(name="comments")
		@Nullable 
		private String comments;

		@XmlElementWrapper(name="category")
		@XmlElement(name="category")
		@Nullable
		private List<String> categories;

		@XmlElementWrapper(name="invokingLanguages")
		@XmlElement(name="invokingLanguage")
		@Nullable
		private List<ResolveTarget> invokingLanguages;

		@XmlElement(name="interface")
		@Nullable
		private UtilityInterface utilityInterface;

		@XmlElementWrapper(name="parameters")
		@XmlElement(name="parameter")
		@Nullable
		private List<UtilityParameter> parameters;

		@XmlElementWrapper(name="steps")
		@XmlElement(name="step")
		@Nullable
		private List<XmlStep> steps;
	}	
}
