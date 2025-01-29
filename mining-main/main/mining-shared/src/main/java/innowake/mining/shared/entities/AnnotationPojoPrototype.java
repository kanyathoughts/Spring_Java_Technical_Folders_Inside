/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Collection;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import org.apache.commons.lang3.builder.ToStringBuilder;

/*
	Annotation
	+	EntityId			module				module,moduleId
	+	ModuleLocation		location
	+	String				name
	+	WorkingState		state
	+	AnnotationType		type
	-	Long				categoryId
	+	String				createdByUserId
	-	String				updatedByUserId
	-	BinaryString		sourceAttachment
	-	String				englishTranslation
	+	List<String>		reasons				reasons
*/

/**
 * Annotation entity request class.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AnnotationPojoPrototype extends MiningSequentialPojoPrototype<AnnotationPojoPrototype> {
	
	public final Definable<EntityId> module = new Definable<>(false, "Annotation.module");
	public final Definable<ModuleLocation> location = new Definable<>(false, "Annotation.location");
	public final Definable<String> name = new Definable<>(false, "Annotation.name");
	public final Definable<WorkingState> state = new Definable<>(false, "Annotation.state");
	public final Definable<AnnotationType> type = new Definable<>(false, "Annotation.type");
	public final Definable<Long> categoryId = new Definable<>(true, "Annotation.categoryId");
	public final Definable<String> createdByUserId = new Definable<>(false, "Annotation.createdByUserId");
	public final Definable<String> updatedByUserId = new Definable<>(false, "Annotation.updatedByUserId");
	public final Definable<BinaryString> sourceAttachment = new Definable<>(true, "Annotation.sourceAttachment");
	public final Definable<String> englishTranslation = new Definable<>(true, "Annotation.englishTranslation");
	public final Definable<Collection<String>> reasons = new Definable<>(false, "Annotation.reasons");
	
	public AnnotationPojoPrototype() {
		super("Annotation");
	}
	
	@JsonAlias("moduleId")
	public AnnotationPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}
	
	public AnnotationPojoPrototype setLocation(final ModuleLocation location) {
		this.location.set(location);
		return this;
	}
	
	public AnnotationPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
	
	public AnnotationPojoPrototype setState(final WorkingState state) {
		this.state.set(state);
		return this;
	}
	
	public AnnotationPojoPrototype setType(final AnnotationType type) {
		this.type.set(type);
		return this;
	}
	
	public AnnotationPojoPrototype setCategoryId(@Nullable final Long categoryId) {
		this.categoryId.set(categoryId);
		return this;
	}
	
	public AnnotationPojoPrototype setCreatedByUserId(final String createdByUserId) {
		this.createdByUserId.set(createdByUserId);
		return this;
	}
	
	@SuppressWarnings("null")
	public AnnotationPojoPrototype setUpdatedByUserId(final String updatedByUserId) {
		/* for backwards compatibility ignore null value send by UI */
		if (updatedByUserId != null) {
			this.updatedByUserId.set(updatedByUserId);
		}

		return this;
	}
	
	public AnnotationPojoPrototype setSourceAttachment(@Nullable final BinaryString sourceAttachment) {
		this.sourceAttachment.set(sourceAttachment);
		return this;
	}
	
	public AnnotationPojoPrototype setSourceAttachment(@Nullable final String sourceAttachment) {
		this.sourceAttachment.set(sourceAttachment != null ? new BinaryString(sourceAttachment) : null);
		return this;
	}
	
	public AnnotationPojoPrototype setEnglishTranslation(@Nullable final String englishTranslation) {
		this.englishTranslation.set(englishTranslation);
		return this;
	}
	
	public AnnotationPojoPrototype setReasons(@Nullable final Collection<String> reasons) {
		this.reasons.set(reasons);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module.orElse(null))
				.append("location", location.orElse(null))
				.append("name", name.orElse(null))
				.append("state", state.orElse(null))
				.append("type", type.orElse(null))
				.append("categoryId", categoryId.orElse(null))
				.append("createdByUserId", createdByUserId.orElse(null))
				.append("updatedByUserId", updatedByUserId.orElse(null))
				.append("sourceAttachment", sourceAttachment.orElse(null))
				.append("englishTranslation", englishTranslation.orElse(null))
				.append("reasons", reasons.orElse(null))
				.toString();
	}
}
