/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.mining.shared.io.WorkbookDefinition.UID;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Abstract base class for {@link RowCallback}.
 */
abstract class AbstractRowCallback implements RowCallback {

	protected static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

	protected final EntityId projectId;
	protected final String fileId;
	private final Map<String, Integer> headers = new HashMap<>(16);

	protected AbstractRowCallback(final EntityId projectId, final String fileId) {
		this.projectId = projectId;
		this.fileId = fileId;
	}

	@Override
	public void setHeaders(final String[] headers) {
		this.headers.clear();

		for (var i = 0; i < headers.length; i++) {
			this.headers.put(headers[i], Integer.valueOf(i));
		}
	}

	/**
	 * Returns the cell value from the given {@code row}. The index of the cell value in {@code row} is calculated by searching for the given
	 * {@code columnHeaders} in the {@code headers} to index mapping.
	 * <p>You can set multiple column headers, if the column header got changed to keep backwards compatibility.
	 * The first column header is considered as to be the latest one.</p>
	 * 
	 * @param row the value row
	 * @param columnHeaders the list of possible column headers of the cell. Set multiple headers if the header got changed for backwards compatibility.
	 * @return the cell value
	 * @throws IllegalArgumentException if the index of the {@code columnHeader} is unknown
	 */
	protected String getMandatoryValue(final String[] row, final String... columnHeaders) {
		final String value = getOptionalValue(row, columnHeaders);
		if (value != null) {
			return value;
		}

		throw new IllegalArgumentException("Unable to import value of column: " + columnHeaders[0]);
	}

	/**
	 * Returns the cell value from the given {@code row}. The index of the cell value in {@code row} is calculated by searching for the given
	 * {@code columnHeaders} in the {@code headers} to index mapping. If the  {@code columnHeader} is unknown, then {@code null} is returned.
	 * <p>You can set multiple column headers, if the column header got changed to keep backwards compatibility.
	 * The first column header is considered as to be the latest one.</p>
	 * 
	 * @param row the value row
	 * @param columnHeaders the list of possible column headers of the cell. Set multiple headers if the header got changed for backwards compatibility.
	 * @return the cell value
	 */
	@Nullable
	protected String getOptionalValue(final String[] row, final String... columnHeaders) {
		if (columnHeaders.length == 0) {
			throw new IllegalArgumentException("At least one column header must be set");
		}

		for (final String columnHeader : columnHeaders) {
			final Integer index = headers.get(columnHeader);
			if (index != null) {
				return index.intValue() < row.length ? row[index.intValue()] : null;
			}
		}

		LOG.debug(() -> String.format("The column %s is not present in the imported sheet", columnHeaders[0]));
		return null;
	}

	/**
	 * Returns the cell value from the given {@code row}. The index of the cell value in {@code row} is calculated by searching for the given
	 * {@code columnHeaders} in the {@code headers} to index mapping. If the  {@code columnHeader} is unknown, then {@code null} is returned.
	 * <p>You can set multiple column headers, if the column header got changed to keep backwards compatibility.
	 * The first column header is considered as to be the latest one.</p>
	 * 
	 * @param row the value row
	 * @param columnHeaders the list of possible column headers of the cell. Set multiple headers if the header got changed for backwards compatibility.
	 * @return the cell value
	 */
	@Nullable
	protected Integer getOptionalIntegerValue(final String[] row, final String... columnHeaders) {
		final String value = getOptionalValue(row, columnHeaders);
		return value == null ? null : Integer.valueOf(value);
	}

	/**
	 * Returns the {@link Technology} for a string.
	 *
	 * @param technology the string
	 * @return the {@link Technology}
	 * @throws IllegalArgumentException if {@link Technology} for the string cannot be resolved
	 */
	protected Technology getTechnology(final String technology) {
		try {
			return Technology.fromName(technology);
		} catch (final IllegalArgumentException e) {
			throw new IllegalArgumentException(String.format("Error while parsing Excel '%s'. Unsupported language '%s'.", fileId, technology), e);
		}
	}
	
	/**
	 * Returns the {@link Type} for a string.
	 *
	 * @param type the string
	 * @return the {@link Type}
	 * @throws IllegalArgumentException if {@link Type} for the string cannot be resolved
	 */
	protected Type getType(final String type) {
		try {
			return Type.fromName(type);
		} catch (final IllegalArgumentException e) {
			throw new IllegalArgumentException(String.format("Error while parsing Excel '%s'. Unsupported type '%s'.", fileId, type), e);
		}
	}
	
	/**
	 * Returns the {@link Storage} for a {@link Technology} and a {@link Type}.
	 *
	 * @param technology the {@link Technology}
	 * @param type the {@link Type}
	 * @return the {@link Storage}
	 * @throws IllegalArgumentException if {@link Storage} for the {@link Technology} and the {@link Type} cannot be resolved
	 */
	protected Storage getStorage(final Technology technology, final Type type) {
		try {
			return Storage.from(technology, type);
		} catch (final IllegalArgumentException e) {
			throw new IllegalArgumentException(String.format("Error while parsing Excel '%s'. "
					+ "Storage for technology '%s' and type '%s' not supported.", fileId, technology, type), e);
		}
	}
	
	/**
	 * Returns the {@link RelationshipType} for a {@link Technology} and a {@link Type}.
	 *
	 * @param technology the {@link Technology}
	 * @param type the {@link Type}
	 * @return the {@link RelationshipType}
	 * @throws IllegalArgumentException if {@link Storage} for the {@link RelationshipType} and the {@link Type} cannot be resolved
	 */
	protected RelationshipType getReferenceType(final Technology technology, final Type type) {
		try {
			return RelationshipType.from(technology, type);
		} catch (final IllegalArgumentException e) {
			throw new IllegalArgumentException(String.format("Error while parsing Excel '%s'. Unsupported relationship of technology '%s' and type '%s'.",
					fileId, technology, type), e);
		}
	}

	/**
	 * Returns the {@linkplain EntityId} of the module by searching for the numeric module id in the given {@code row} and then loading the module {@link UUID}
	 * from the given {@code modulesNidToEid} map.
	 * <p>If {@code modulesNidToEid} contains no entry for the numeric module id, then an {@linkplain IllegalArgumentException} is thrown.
	 * 
	 *
	 * @param modulesNidToEid map containing the mapping of module numeric to {@link EntityId EntityIds}
	 * @param row the value row
	 * @return the module {@linkplain EntityId}
	 */
	protected EntityId getModuleId(final Map<Long, EntityId> modulesNidToEid, final String[] row) {
		/* The column name in the excel sheet and CSV file is called "UID" but in Postgres it is the nid */
		final var nid = Long.valueOf(getMandatoryValue(row, UID));
		final var id = modulesNidToEid.get(nid);
		if (id == null) {
			throw new IllegalArgumentException("Unable to get module id for nid: " + nid);
		}

		return id;
	}
}
