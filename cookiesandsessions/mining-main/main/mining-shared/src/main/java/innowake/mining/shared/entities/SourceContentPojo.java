/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * A simplified {@link SourcePojo} for only the content with path and ID.
 */
@MiningDataType(name = MiningEnitityNames.SOURCE_CONTENT)
public class SourceContentPojo {
	
	private final EntityId id;
	private final String path;
	private final BinaryString content;
	
	public SourceContentPojo(final EntityId id, final String path, final BinaryString content) {
		super();
		this.id = id;
		this.path = path;
		this.content = content;
	}
	
	public EntityId getId() {
		return id;
	}
	
	public String getPath() {
		return path;
	}
	
	public BinaryString getContent() {
		return content;
	}
	
}
