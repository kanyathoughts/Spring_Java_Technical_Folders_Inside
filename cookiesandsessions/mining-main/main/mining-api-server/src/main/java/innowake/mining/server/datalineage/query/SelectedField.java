/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.query;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;

import java.io.Serializable;
import java.util.Optional;

/**
 * Represents a field in a module selected for data lineage. To be used with {@link Parameters}.
 * <p>
 * A field is identified via its module id and the offset within the source code where the field is located. The offset is either interpreted
 * as an assembled or a retraced offset, depending on the {@link Parameters#isAssembled()} setting. If the selected field is inside a copybook module,
 * then additionally the {@code includingModule} can be specified to limit data lineage analysis of the field to that module only. If not specified,
 * all modules that include the copybook field will be analyzed.
 */
public class SelectedField implements Serializable {

	private final EntityId module;
	private final int offset;
	@Nullable
	private final EntityId includingModule;

	/**
	 * Selects the field in the given module at the given offset.
	 * <p>
	 * See the descriptions of the getters for details how the parameters are interpreted.
	 *
	 * @param module id of the module containing the field
	 * @param offset offset of the field within the module
	 */
	public SelectedField(final EntityId module, final int offset) {
		this.module = module;
		this.offset = offset;
		includingModule = null;
	}

	/**
	 * Selects the field in the given module at the given offset when included by given including module.
	 * <p>
	 * See the descriptions of the getters for details how the parameters are interpreted.
	 *
	 * @param module id of the module containing the field
	 * @param offset offset of the field within the module
	 * @param includingModule the id another module that includes {@code module}
	 */
	public SelectedField(final EntityId module, final int offset, @Nullable final EntityId includingModule) {
		this.module = module;
		this.offset = offset;
		this.includingModule = includingModule;
	}

	/**
	 * Gets the id of the module where the field resides. In {@linkplain Parameters#isAssembled() "assembled mode"} this is the id of the module
	 * that is the "root" of the assembling - i.e. the program. In not-assembled mode, this is the id of the module where the field physically
	 * (i.e. in source code) resides.
	 * @return the id of the module containing the field
	 */
	public EntityId getModule() {
		return module;
	}

	/**
	 * The offset of the field within the source code. In {@linkplain Parameters#isAssembled() "assembled mode"} the offset is interpreted as an assembled
	 * offset, so denotes the location of the field in the final assembled program. In not-assembled mode the offset is the physical offset of the field
	 * in the source code of the module.
	 * @return the field offset
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * If present, limits data lineage analysis to usages of the field only in the given module. When not present and a field from a copybook is selected,
	 * then by default all modules that include the copybook are analyzed.
	 * <p>
	 * Only applicable when not using {@linkplain Parameters#isAssembled() "assembled mode"} and the field is located in a copybook.
	 * @return the including module id
	 */
	public Optional<EntityId> getIncludingModule() {
		return Optional.ofNullable(includingModule);
	}
}
