import { JobInformationStatusEnum } from 'mining-api-client';
import { AxiosResponse } from 'axios';
import { withDefaultErrorHandling } from '../default-error-handling';
import { terminal } from 'terminal-kit';
import { ProgressBarController } from 'terminal-kit/Terminal';
import { JobControllerApi } from 'mining-api-client';
import { IOC } from 'iw-ioc';
import { writeFile } from 'fs-extra';
import byteSize from 'byte-size';

const REFRESH_INTERVAL = 2000;

/* states in which we consider a job "failed" */
const FAILED_JOB_STATES: string[] = [
    JobInformationStatusEnum.FAILURE,
    JobInformationStatusEnum.TIMEOUT,
    JobInformationStatusEnum.CANCELED
];

/* states in which we consider a job "finished" */
const FINISHED_JOB_STATES: string[] = [
    ...FAILED_JOB_STATES,
    JobInformationStatusEnum.SUCCESS
];

const RUNNING_JOB_STATES: string[] = [
    JobInformationStatusEnum.UNKNOWN,
    JobInformationStatusEnum.SCHEDULED,
    JobInformationStatusEnum.RUNNING,
    JobInformationStatusEnum.CANCEL_REQUESTED
];

export interface JobActionOptions {
    title: string;
    downloadResult?: boolean;
}

export function jobAction<T extends any[]>(
    options: JobActionOptions,
    actionFn: (...args: T) => Promise<AxiosResponse<string | string[]>>,
    resultFn?: (jobId: string, jobStatus: JobInformationStatusEnum) => any
): (...args: T) => Promise<void> {
    return withDefaultErrorHandling(async (...args: T) => {
        const jobControllerApi = IOC.get(JobControllerApi)!;
        const response = await actionFn(... args);
        const jobId = response.data as string; /* swagger interprets Java return type char[] as string[] -.- */

        console.log('Submitted job', jobId);

        const progressBar = terminal.progressBar({
            width: terminal.width,
            title: options.title,
            /* use golden ratio for <title|progress bar> layout for maximum eye pleasure
             * but also ensure that title gets 80 characters at minimum
             * but also ensure to shrink title on small terminals so there's at least 20 characters left for progress bar
             * because otherwise the terminal-kit library will crash (empirically tested, WMIN-3875) */
            titleSize: Math.min(Math.max(80, terminal.width * 0.618), terminal.width - 20),
            percent: true,
            eta: true
        });

        let jobStatus: JobInformationStatusEnum = JobInformationStatusEnum.UNKNOWN;
        try {
            jobStatus = await updateJobStatus(jobControllerApi, jobId, progressBar);
        } finally {
            progressBar.stop();
            terminal.eraseLine();
        }
        console.log('Job finished', jobStatus);

        if (options.downloadResult) {
            const result = await jobControllerApi.getJobResult(jobId, { responseType: 'arraybuffer' });
            if (result.status === 200) {
                const data = Buffer.from(result.data as any);
                const dispo: string = result.headers['content-disposition'];
                if (dispo) {
                    const filename = dispo ? dispo.substring(dispo.lastIndexOf('filename=') + 'filename='.length) : `${jobId}.json`;
                    await writeFile(filename, data);
                    console.log(`Downloaded ${filename} (${byteSize(data.length)})`);
                } else {
                    /* Content-Disposition indicates that a file should be downloaded - when not present we just echo the Job results on the console */
                    console.log(data.toString('utf8'));
                }
            }
        }

        await resultFn?.(jobId, jobStatus);
    });
}

function updateJobStatus(jobControllerApi: JobControllerApi, jobId: string, progressBar: ProgressBarController): Promise<JobInformationStatusEnum> {

    return new Promise<JobInformationStatusEnum>(async (resolve, reject) => {
        const doUpdate = async () => {
            try {
                const jobStatus = (await jobControllerApi.getJobInformation(jobId)).data;
                progressBar.update({
                    title: jobStatus.stepDescription ?? jobStatus.jobDescription,
                    progress: (jobStatus.processedWorkUnits ?? 0) / (jobStatus.totalWorkUnits ?? 1)
                });
                if (FINISHED_JOB_STATES.includes(jobStatus.status ?? JobInformationStatusEnum.UNKNOWN)) {
                    resolve(jobStatus.status ?? JobInformationStatusEnum.UNKNOWN);
                } else {
                    setTimeout(() => doUpdate(), REFRESH_INTERVAL);
                }
            } catch (err) {
                reject(err);
            }
        };
        doUpdate();
    });
}