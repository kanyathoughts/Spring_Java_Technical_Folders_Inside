package innowake.mining.data.migration.postgres;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.sql.SQLException;
import java.util.List;
import java.util.UUID;

import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.lib.job.api.Message;

@SuppressWarnings("removal")
public class JobInfoMigration extends PostgresSchemaMigrationFromOrient {

	public JobInfoMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("job_info_table_creation");

		/* To compare Pg migrated times with the once in OrientDB:
		 * SELECT jobName, submitTime.format("yyyy-MM-dd HH:mm:ss:SSS") as submitTime, scheduledStartTime.format("yyyy-MM-dd HH:mm:ss:SSS") as scheduledStartTime, 
		 * startTime.format("yyyy-MM-dd HH:mm:ss:SSS") as startTime, finishTime.format("yyyy-MM-dd HH:mm:ss:SSS") as finishTime FROM jobInfo */
		migrateData("JobInfo",
			"SELECT jobId, jobName, jobDescription, stepDescription, status, pendingTasks, totalWorkUnits, processedWorkUnits,"
					+ "submitTime, scheduledStartTime, startTime, finishTime, userName FROM jobInfo",
			"INSERT INTO job_info values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
			1000, (in, out, n) -> {
				out.add(UUID.fromString(in.getString(1))); /* id */
				out.add(in.getString(2)); /* name */
				out.add(in.getString(3)); /* description */
				out.add(in.getString(4)); /* step_description */
				out.add(in.getString(5)); /* status */
				out.add(in.getInt(6)); /* pending_tasks */
				out.add(in.getInt(7)); /* total_work_units */
				out.add(in.getDouble(8)); /* processed_work_units */
				out.add(in.getTimestamp(9)); /* submit_time */
				out.add(in.getTimestamp(10)); /* scheduled_start_time */
				out.add(in.getTimestamp(11)); /* start_time */
				out.add(in.getTimestamp(12)); /* finish_time */
				out.add(in.getString(13)); /* created_by */

				return false;
			});

		migrateData("JobInfo messages",
				"SELECT jobId, messages FROM jobInfo WHERE messages IS NOT NULL",
				"INSERT INTO job_info_messages values (?, ?, ?, ?)",
				1000, (in, out, n) -> {
					final List<Message> messages = deserializeObject(in.getBytes(2));
					if ( ! messages.isEmpty()) {
						out.add(UUID.fromString(in.getString(1))); /* id */
						out.add(n + 1); /* ordinal */

						final Message message = messages.get(n);
						out.add(message.getText()); /* text */
						out.add(message.getSeverity().name()); /* severity */
					}
					return n < messages.size() - 1;
				});

		migrateData("JobInfo result",
				"SELECT jobId, result FROM jobInfo WHERE result IS NOT NULL",
				"INSERT INTO job_info_results values (?, ?, ?, ?, ?)",
				1000, (in, out, n) -> {
					out.add(UUID.fromString(in.getString(1))); /* id */
					out.add("$result"); /* name */
					out.add("application/x-java-object"); /* type */
					out.add("innowake.lib.job.api.Result"); /* class */
					out.add(in.getBytes(2)); /* content */
					return false;
				});
	}

	@SuppressWarnings("unchecked")
	private static <T> T deserializeObject(final byte[] bytes) {
		try (final ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
			 final ObjectInputStream ois = new ObjectInputStream(bais)) {
			return (T) ois.readObject();
		} catch (final IOException | ClassNotFoundException e) {
			LOG.error("Unable to deserialize object", e);
			throw new IllegalStateException("Unable to deserialize object", e);
		}
	}
}
