/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signalling that the Taxonomies for a project were modified (created/updated/deleted).
 * <p>
 * This event is fired when Taxonomies, TaxonomyTypes or TaxonomyCategories are modified, 
 * but is <strong>not</strong> fired when Taxonomy assignments are changed
 * (i.e. when a Taxonomy is added or removed from a Module).
 */
public class TaxonomiesModifiedEvent extends ProjectSpecificEvent {
	
	public TaxonomiesModifiedEvent(final Long projectId) {
		this(EntityId.of(projectId));
	}
	
	public TaxonomiesModifiedEvent(final EntityId projectId) {
		super(Optional.of(projectId));
	}
}
