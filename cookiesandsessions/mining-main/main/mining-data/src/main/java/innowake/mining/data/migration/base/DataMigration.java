/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Hauls data from one table/database to another.
 */
public abstract class DataMigration implements Runnable {
	
	/**
	 * see {@link #map(ResultSet, List, int)}
	 */
	public interface RecordMapper {
		/**
		 * Maps a source record to one or more target record(s).
		 * @param in ResultSet positioned on the current input record.
		 * @param out List of values constituting the output record. If this is left empty no output record will be written.
		 * @param pass Number of records already written for the same input record.
		 * @return Whether there are more records to write for the same input.
		 *         If this is true the mapper will be called again with the same input record and <i>pass</i> incremented.
		 * @throws SQLException If an unrecoverable mapping issue occurs.
		 */
		public boolean map(final ResultSet in, final List<Object> out, final int pass) throws SQLException;
	}
	
	private final String title;
	private final Connection dbIn;
	private final Connection dbOut;
	private final int maxBatchSize;
	
	private final AtomicInteger recordsRead;
	private final AtomicInteger recordsWritten;
	private final AtomicInteger currentBatchSize;
	
	private static final Logger LOG = LoggerFactory.getLogger(innowake.mining.data.Logging.MIGRATION);
	
	/**
	 * Initializes a new data migration.
	 * @param title Arbitrary title for logging.
	 * @param dbRead Database connection to read from.
	 * @param dbWrite Database connection to write to.
	 * @param maxBatchSize Maximum number of records to be transfered in a single request.
	 */
	protected DataMigration(final String title, final Connection dbRead, final Connection dbWrite, final int maxBatchSize) {
		super();
		this.title = title;
		this.dbIn = dbRead;
		this.dbOut = dbWrite;
		this.maxBatchSize = maxBatchSize;
		this.recordsRead = new AtomicInteger();
		this.recordsWritten = new AtomicInteger();
		this.currentBatchSize = new AtomicInteger();
	}
	
	/**
	 * Prepares the statement for reading input records.
	 * @param db Database connection to read from.
	 * @return Executable PreparedStatement providing the records to migrate.
	 * @throws SQLException If anything goes wrong.
	 */
	protected abstract PreparedStatement prepareRead(final Connection db) throws SQLException;
	
	/**
	 * Prepares the statement for writing output records.
	 * @param db Database connection to write to.
	 * @return PreparedStatement with parameters corresponding to the fields in the migrated output records.
	 * @throws SQLException If anything goes wrong.
	 */
	protected abstract PreparedStatement prepareWrite(final Connection db) throws SQLException;
	
	/**
	 * Provides the implementation for mapping input to output records.
	 * @return RowMapper to perform the migration of each record.
	 * @throws SQLException If anything goes wrong.
	 */
	protected abstract RecordMapper provideMapper() throws SQLException;
	
	/**
	 * Gets the number of input records.
	 * @return Number of records read from the input statement.
	 */
	public int getReadCount() {
		return recordsRead.get();
	}
	
	/**
	 * Convenience method for setting up a data migration.
	 * @param title Arbitrary title for logging.
	 * @param dbIn Database connection to read from.
	 * @param readQuery Query for reading records from the source.
	 * @param dbOut Database connection to write to.
	 * @param writeQuery Query for writing records at the destination.
	 * @param batchSize Maximum number of records to be transfered in a single request.
	 * @param mapper Procedure for converting data field between source and destination. 
	 * @return New data migration process.
	 */
	public static DataMigration setup(final String title, final Connection dbIn, final String readQuery,
			final Connection dbOut, final String writeQuery, final int batchSize, final RecordMapper mapper) {
		return new DataMigration(title, dbIn, dbOut, batchSize) {
			@Override
			protected PreparedStatement prepareRead(final Connection db) throws SQLException {
				return db.prepareStatement(readQuery);
			}
			@Override
			protected PreparedStatement prepareWrite(final Connection db) throws SQLException {
				return db.prepareStatement(writeQuery);
			}
			@Override
			protected RecordMapper provideMapper() throws SQLException {
				return mapper;
			}
		};
	}
	
	private void doBatch(final PreparedStatement st) throws SQLException {
		final AtomicLong timeRef = new AtomicLong(System.nanoTime());
		for (final int n : st.executeBatch()) {
			if (n >= 0 || n == Statement.SUCCESS_NO_INFO) {
				recordsWritten.incrementAndGet();
			} else {
				throw new SQLException("Batch statement returned no count.");
			}
		}
		timeRef.set(System.nanoTime() - timeRef.get());
		currentBatchSize.set(0);
		LOG.info(() -> title + String.format(": %d->%d in %.3fs", recordsRead.get(), recordsWritten.get(), timeRef.get() / 1000000000.0));
	}
	
	@Override
	public void run() {
		LOG.info(() -> title + " migration started");
		final AtomicLong timeRef = new AtomicLong(System.currentTimeMillis());
		try (
			final PreparedStatement statementRead = prepareRead(dbIn);
			final PreparedStatement statementWrite = prepareWrite(dbOut);
		) {
			statementRead.setFetchSize(maxBatchSize);
			final int paramCount = statementWrite.getParameterMetaData().getParameterCount();
			final ResultSet inResult = statementRead.executeQuery();
			RecordMapper mapper = provideMapper();
			while (inResult.next()) {
				recordsRead.incrementAndGet();
				int rowsFromSameInput = 0;
				boolean moreFromSameInput;
				do {
					List<Object> outArgs = new ArrayList<>(paramCount);
					moreFromSameInput = mapper.map(inResult, outArgs, rowsFromSameInput);
					rowsFromSameInput++;
					if (! outArgs.isEmpty()) {
						if (outArgs.size() != paramCount) {
							throw new SQLException("Invalid number of output arguments (defined=" + outArgs.size() + ", expected=" + paramCount + ").");
						}
						
						for (int n = 1; n <= paramCount; n++) {
							statementWrite.setObject(n, outArgs.get(n - 1));
						}
						
						statementWrite.addBatch();
						currentBatchSize.incrementAndGet();
						if (currentBatchSize.get() >= maxBatchSize) {
							doBatch(statementWrite);
						}
					}
				} while (moreFromSameInput);
			}
			doBatch(statementWrite);
		} catch (final Exception e) {
			throw new IllegalStateException(title
					+ " (read: " + recordsRead.get() + ", written: " + recordsWritten.get() + ", batch: " + currentBatchSize.get() + ")", e);
		} finally {
			timeRef.set(System.currentTimeMillis() - timeRef.get());
			LOG.info(() -> title + String.format(" migration done in %.3fs (%.3f/s)", 
					timeRef.get() / 1000.0, recordsWritten.get() / (timeRef.get() / 1000.0)));
		}
	}
	
}
