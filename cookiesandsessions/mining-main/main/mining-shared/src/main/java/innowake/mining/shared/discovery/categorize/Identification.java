/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.categorize;

import static innowake.mining.shared.model.discovery.ResolveTarget.NONE;

import java.io.Serializable;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

public class Identification implements Serializable {
	
	public enum ID { NO, MAYBE, YES }
	
	private ID id;
	public final Long resourceId;
	private ResolveTarget target;
	private ResolveTarget language;
	@Nullable
	private final String newName;

	/**
	 * Constructor.
	 * 
	 * @param id the {@link ID}
	 * @param resourceId the Id of the {@link SourcePojo}
	 * @param target the type of the identification
	 * @param language the language of the detector that created this identification
	 */
	public Identification(final ID id, final Long resourceId, final ResolveTarget target, final ResolveTarget language) {
		this(id, resourceId, target, language, null);
	}

	/**
	 * Constructor.
	 * 
	 * @param id the {@link ID}
	 * @param resourceId the Id of the {@link SourcePojo}
	 * @param target the type of the identification
	 * @param language the language of the detector that created this identification
	 * @param newName the new name of the {@link SourcePojo} after the move operation
	 */
	public Identification(final ID id, final Long resourceId, final ResolveTarget target, final ResolveTarget language, @Nullable final String newName) {
		this.id = id;
		this.resourceId = resourceId;
		this.target = target;
		this.language = language;
		this.newName = newName;
	}
	
	public ID getId() {
		return id;
	}
	
	public ResolveTarget getTarget() {
		return target;
	}
	
	public ResolveTarget getCategorizerLanguage() {
		return language;
	}
	
	public void setId(final ID id) {
		this.id = id;
	}
	
	public void setTarget(final ResolveTarget target) {
		this.target = target;
	}
	
	public static Identification notIdentified(final SourcePojo resource, final ResolveTarget language) {
		return new Identification(ID.NO, resource.getId(), NONE, language);
	}

	public Long getResourceId() {
		return resourceId;
	}

	/**
	 * @return the new name of the {@link SourcePojo} after the move operation
	 */
	@Nullable
	public String getNewName() {
		return newName;
	}
	
	@Override
	public String toString() {
		return String.format("%s %s %d", id, target, resourceId);
	}
	

}
