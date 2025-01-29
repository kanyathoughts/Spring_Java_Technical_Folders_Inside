/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobInformation;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.entities.JobInfoPojoPrototype;

/**
 * Utility for job information classes. Can be used for serialization and deserialization of job results.
 */
public class JobInfoUtil {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.PERSISTENCE);

	private JobInfoUtil() {
		/* no instances allowed */
	}

	/**
	 * Returns a new instance of {@link JobInformation} based on the given {@link JobInfoPojo}.
	 *
	 * @param jobInfo the {@link JobInfoPojo} to convert
	 * @return the {@link JobInformation} instance
	 */
	public static JobInformation toJobInformation(final JobInfoPojo jobInfo) {
		return new JobInfoWrapper(jobInfo);
	}

	/**
	 * Converts the given {@link JobInformation} instance to a {@link JobInfoPojoPrototype}.
	 *
	 * @param jobInfo the {@link JobInformation} instance to convert
	 * @return the {@link JobInfoPojoPrototype} instance
	 */
	public static JobInfoPojoPrototype toPrototype(final JobInformation jobInfo) {
		final JobInfoPojoPrototype prototype = new JobInfoPojoPrototype()
				.setId(jobInfo.getId())
				.setName(jobInfo.getJobName())
				.setCreatedByUserId(jobInfo.getUserName())
				.setSubmitTime(jobInfo.getSubmitTime())
				.setStatus(jobInfo.getStatus())
				.setPendingTasks(jobInfo.getPendingTasks())
				.setTotalWorkUnits(jobInfo.getTotalWorkUnits())
				.setProcessedWorkUnits(jobInfo.getProcessedWorkUnits());

		if (jobInfo.getJobDescription() != null) {
			prototype.setDescription(jobInfo.getJobDescription());
		}
		if (jobInfo.getStepDescription() != null) {
			prototype.setStepDescription(jobInfo.getStepDescription());
		}
		if (jobInfo.getScheduledStartTime() != null) {
			prototype.setScheduledStartTime(jobInfo.getScheduledStartTime());
		}
		if (jobInfo.getStartTime() != null) {
			prototype.setStartTime(jobInfo.getStartTime());
		}
		if (jobInfo.getFinishTime() != null) {
			prototype.setFinishTime(jobInfo.getFinishTime());
		}

		return prototype;
	}

	@SuppressWarnings("unchecked")
	@Nullable
	public static <T> T deserializeObject(@Nullable final byte[] bytes) {
		if (bytes != null) {
			try (final ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
				 final ObjectInputStream ois = new ObjectInputStream(bais)) {
				return (T) ois.readObject();
			} catch (final IOException | ClassNotFoundException e) {
				LOG.error("Unable to deserialize object", e);
			}
		}

		return null;
	}

	public static <T> byte[] serializeObject(final T object) {
		try (final ByteArrayOutputStream baos = new ByteArrayOutputStream();
			 final ObjectOutputStream oos = new ObjectOutputStream(baos)) {
			oos.writeObject(object);
			oos.flush();
			final byte[] result = baos.toByteArray();
			LOG.debug(() -> "Serialize object with " + result.length + " bytes");
			return result;
		} catch (final IOException e) {
			throw new IllegalStateException("Unable to serialize object: " + object, e);
		}
	}
}
