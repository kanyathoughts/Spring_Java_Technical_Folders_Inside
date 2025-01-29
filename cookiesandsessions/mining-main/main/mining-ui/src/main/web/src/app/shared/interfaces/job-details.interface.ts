import { JobInformation } from '@innowake/mining-api-angular-client';

const moduleDetailsUrl = 'details/overview';

export interface JobDetails extends JobInformation {
    jobIcon?: string,
    stepDescription?: string,
    actionLabel?: string,
    colorClass?: string,
    jobStartedBy?: string,
    time?: Date | string,
    details?: any,
    jobType?: string,
    downloadLink?: any,
    loadingLogButton?: boolean,
    disableActionButton?: boolean,
    listingPriority?: number,
    resultButton?: string;
    projectId?: number;
    moduleId?: number
}

/** export the job type constant to be used for routing to respective pages. */
export const JOB_TYPE = {
  'Discover Code': { urlParams: 'metrics/summary', routeType: 'project' },
  'Discover DNA': { urlParams: 'dna', routeType: 'project' },
  'Discover Metrics': { urlParams: 'metrics/summary', routeType: 'project' },
  'Taxonomy Import': { urlParams: 'configuration/taxonomies', routeType: 'project' },
  'Calculate Control Flow Graph': { urlParams: 'control-flow', routeType: 'module' },
  'Taxonomy Assignment': { urlParams: moduleDetailsUrl, routeType: 'module' },
  'Identify Technical Taxonomies': { urlParams: moduleDetailsUrl, routeType: 'module' },
  'Identify Candidates': { urlParams: 'metrics/candidates', routeType: 'project' },
  'Identify Module Descriptions': { urlParams: moduleDetailsUrl, routeType: 'module' },
  'Generate Reachability Block Description': { urlParams: '', routeType: '', showModal: true },
  'Control M Import': { urlParams: 'configuration/scheduler-data', routeType: 'project' }
};

