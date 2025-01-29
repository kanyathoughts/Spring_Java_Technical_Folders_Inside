/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

import innowake.lib.core.api.lang.Nullable;

import java.util.Objects;
import java.util.UUID;

/**
 * POJO for SchedulerEntryRelationship
 */
public class SchedulerEntryRelationshipPojo {
	private final UUID uid;
	private final UUID predecessor;
	private final UUID successor;
	private final boolean isOk;
	@Nullable
	private final String identifier;
	@Nullable
	private final UUID predecessorModule;
	@Nullable
	private final UUID successorModule;

	/**
	 * Constructor
	 *
	 * @param uid UUID
	 * @param predecessor the predecessor entry UID
	 * @param successor the successor entry UID
	 */
	public SchedulerEntryRelationshipPojo(final UUID uid, final UUID predecessor, final UUID successor) {
		this(uid, predecessor, successor, true, null, null, null);
	}

	/**
	 * Constructor
	 *
	 * @param uid UUID
	 * @param predecessor the predecessor entry UID
	 * @param successor the successor entry UID
	 * @param isOk true if the relationship is established after the predecessor task completed successfully, false otherwise
	 * @param identifier the relationship identifier
	 */
	public SchedulerEntryRelationshipPojo(final UUID uid, final UUID predecessor, final UUID successor, final boolean isOk, final String identifier) {
		this(uid, predecessor, successor, isOk, identifier, null, null);
	}

	/**
	 * Constructor
	 *
	 * @param uid UUID
	 * @param predecessor the predecessor entry UID
	 * @param successor the successor entry UID
	 * @param isOk true if the relationship is established after the predecessor task completed successfully, false otherwise
	 * @param identifier the relationship identifier
	 * @param predecessorModule the predecessor module UID
	 * @param successorModule the successor module UID
	 */
	public SchedulerEntryRelationshipPojo(final UUID uid, final UUID predecessor, final UUID successor, final boolean isOk, @Nullable final String identifier, 
			@Nullable final UUID predecessorModule, @Nullable final UUID successorModule) {
		this.uid = uid;
		this.predecessor = predecessor;
		this.successor = successor;
		this.isOk = isOk;
		this.identifier = identifier;
		this.predecessorModule = predecessorModule;
		this.successorModule = successorModule;
	}

	public UUID getUid() {
		return uid;
	}

	public UUID getPredecessor() {
		return predecessor;
	}

	public UUID getSuccessor() {
		return successor;
	}

	public boolean isOk() {
		return isOk;
	}

	@Nullable
	public String getIdentifier() {
		return identifier;
	}

	@Nullable
	public UUID getPredecessorModule() {
		return predecessorModule;
	}

	@Nullable
	public UUID getSuccessorModule() {
		return successorModule;
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof SchedulerEntryRelationshipPojo)) {
			return false;
		}

		final SchedulerEntryRelationshipPojo that = (SchedulerEntryRelationshipPojo) o;
		return isOk == that.isOk && Objects.equals(predecessor, that.predecessor) && Objects.equals(successor, that.successor) && Objects.equals(identifier,
				that.identifier) && Objects.equals(predecessorModule, that.predecessorModule) && Objects.equals(successorModule, that.successorModule);
	}

	@Override
	public int hashCode() {
		return Objects.hash(predecessor, successor, isOk, identifier, predecessorModule, successorModule);
	}

	@Override
	public String toString() {
		return "SchedulerEntryRelationshipPojo{" +
				"uid=" + uid +
				", predecessor=" + predecessor +
				", successor=" + successor +
				", isOk=" + isOk +
				", identifier='" + identifier + '\'' +
				", predecessorModule=" + predecessorModule +
				", successorModule=" + successorModule +
				'}';
	}
}
