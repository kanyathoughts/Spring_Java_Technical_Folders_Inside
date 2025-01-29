package innowake.lib.job.api.management;

/**
 * Provides information of a discovery job that is either currently being
 * executed or already finished.
 */
public class JobLog {

        private String jobId;
        private String log;

        public JobLog(final String jobId, final String log) {
                this.jobId = jobId;
                this.log = log;
        }

        public String getLog() {
                return log;
        }

        public String getJobId() {
                return jobId;
        }
}