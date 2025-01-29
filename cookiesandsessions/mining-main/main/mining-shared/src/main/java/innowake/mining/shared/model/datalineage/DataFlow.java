/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;

import java.util.Objects;

/**
 * Describes outgoing dataflow from a {@link DataFlowNodePojo} to another {@code DataFlowNode} given as {@linkplain #getDestination() destination}.
 * The dataflow may also reference the {@linkplain #getStatement() statement} that initiates the data flow. Finally, a dataflow may also reference
 * a {@linkplain #getStatement() statement} without a {@linkplain #getDestination() destination}, indicating that data is consumed by the statement
 * but not passed on further.
 */
public class DataFlow {

	@Nullable
	private final DataFlowId destination;
	@Nullable
	private final DataFlowId statement;

	/**
	 * Constructor.
	 * @param destination {@link DataFlowId} for destination
	 * @param statement {@link DataFlowId} for statement
	 */
	private DataFlow(@Nullable final DataFlowId destination, @Nullable final DataFlowId statement) {
		this.destination = destination;
		this.statement = statement;
	}

	/**
	 * Creates Data Flow to Field without statement
	 *
	 * @param destination {@link DataFlowId} for destination
	 * @return a Data Flow to Field without statement
	 */
	public static DataFlow toField(final DataFlowId destination) {
		return new DataFlow(destination, null);
	}

	public static DataFlow toField(final DataFlowNodePrototype destination) {
		return toField(destination.getDataFlowId());
	}

	public static DataFlow toField(final DataFlowNodePojo destination) {
		return toField(destination.getDataFlowId());
	}

	/**
	 * Creates a data flow to Field from Statement 
	 *
	 * @param destination {@link DataFlowId} for destination
	 * @param statement {@link DataFlowId} for statement
	 * @return data flow to Field from Statement
	 */
	public static DataFlow toFieldViaStatement(final DataFlowId destination, final DataFlowId statement) {
		return new DataFlow(destination, statement);
	}

	public static DataFlow toFieldViaStatement(final DataFlowNodePrototype destination, final DataFlowNodePrototype statement) {
		return toFieldViaStatement(destination.getDataFlowId(), statement.getDataFlowId());
	}

	public static DataFlow toFieldViaStatement(final DataFlowNodePojo destination, final DataFlowNodePojo statement) {
		return toFieldViaStatement(destination.getDataFlowId(), statement.getDataFlowId());
	}

	/**
	 * Creates a Data flow to statement without Field
	 *
	 * @param statement {@link DataFlowId} for statement
	 * @return a Data flow to statement without Field
	 */
	public static DataFlow toStatement(final DataFlowId statement) {
		return new DataFlow(null, statement);
	}

	public static DataFlow toStatement(final DataFlowNodePrototype statement) {
		return toStatement(statement.getDataFlowId());
	}

	public static DataFlow toStatement(final DataFlowNodePojo statement) {
		return toStatement(statement.getDataFlowId());
	}

	/**
	 * Returns destination {@link DataFlowId}
	 *
	 * @return destination {@link DataFlowId}
	 */
	@Nullable
	public DataFlowId getDestination() {
		return destination;
	}

	/**
	 * Returns statement {@link DataFlowId}
	 *
	 * @return statement {@link DataFlowId}
	 */
	@Nullable
	public DataFlowId getStatement() {
		return statement;
	}

	@Override
	public boolean equals(final @Nullable Object o) {
		if (this == o) return true;
		if (!(o instanceof DataFlow)) return false;
		final var dataFlow = (DataFlow) o;
		return Objects.equals(destination, dataFlow.destination) && Objects.equals(statement, dataFlow.statement);
	}

	@Override
	public int hashCode() {
		return Objects.hash(destination, statement);
	}
}
