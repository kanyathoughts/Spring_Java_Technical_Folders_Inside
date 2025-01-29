import { JobInformation } from '@innowake/mining-api-angular-client';
import { BehaviorSubject } from 'rxjs';

/**
 * Interface for Remote job related classes
 */
export interface RemoteJob {
  jobId: string;
  uiToken?: string;
  jobInfo?: JobInformation;
  label: string;
  foreground: boolean;
  cancellable: boolean;
  autoDownloadResult: boolean;
  status$: BehaviorSubject<JobInformation.StatusEnum>;
  details?: Map<string, string>;
}
