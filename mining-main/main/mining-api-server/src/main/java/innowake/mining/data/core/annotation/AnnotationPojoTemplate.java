/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import java.util.List;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;


public class AnnotationPojoTemplate extends AnnotationPojoPrototype {
	
	public Definable<List<EntityId>> dataDictionaryReferences = new Definable<>(false, "dataDictionaryReferences");
	
}
