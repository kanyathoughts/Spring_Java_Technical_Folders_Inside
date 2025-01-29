/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.entities.SourcePojo;

/**
 * Handles build post processing steps.
 * <p>
 * This class was widely copied from {@code innowake.ndt.natclipse.core.object.internal.PrjPostProcessingManager}.
 */
class NaturalPostProcessingManager {

	private final Map<SourcePojo, NaturalPostProcessingItem> map = new HashMap<>();
	private final List<NaturalPostProcessingItem> items = new ArrayList<>();
	
	void putToAssemble(final SourcePojo object, final int type) {
		NaturalPostProcessingItem item = map.get(object);
		if (item == null) {
			item = new NaturalPostProcessingItem(object);
			map.put(object, item);
			items.add(item);
		}
		item.putAssemblingType(type);
		if ( ! item.hasToAssemble()) {
			item.putTask(NaturalPostProcessingItem.TASK_ASSEMBLE);
		}
	}

	void clear() {
		map.clear();
		items.clear();
	}
	
	NaturalPostProcessingItem[] getItems() {
		return items.toArray(new NaturalPostProcessingItem[0]);
	}
}
