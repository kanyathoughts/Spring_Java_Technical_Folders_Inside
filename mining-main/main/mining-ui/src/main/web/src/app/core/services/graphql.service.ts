import { Injectable } from '@angular/core';
import {
    HttpClient, HttpHeaders, HttpParams,
    HttpResponse, HttpEvent, HttpUrlEncodingCodec
} from '@angular/common/http';

import { Observable } from 'rxjs';
import { getBasePath } from '../utils/base-path.utils';

@Injectable()
export class GraphQlControllerService {

    public defaultHeaders = new HttpHeaders();
    protected basePath = 'https://localhost:8080';

    constructor(protected httpClient: HttpClient) {
      this.basePath = getBasePath();
    }

    /**
     * Executes a GraphQL query.
     * @param dataPointsQuery the query generated for the list of dataPoints.
     * @param projectId the ID of the project
     * @param observe set whether or not to return the data Observable as the body, response or events. defaults to returning the body.
     * @param reportProgress flag to report request and response progress.
     */
    public graphQl(dataPointsQuery: { [key: string]: any }, projectId?: number, observe?: 'body', reportProgress?: boolean): Observable<string[]>;
    public graphQl(dataPointsQuery: { [key: string]: any }, projectId?: number, observe?: 'response',
                   reportProgress?: boolean): Observable<HttpResponse<string[]>>;
    public graphQl(dataPointsQuery: { [key: string]: any }, projectId?: number, observe?: 'events', reportProgress?: boolean): Observable<HttpEvent<string[]>>;
    public graphQl(dataPointsQuery: { [key: string]: any }, projectId?: number, observe: any = 'body', reportProgress: boolean = false): Observable<any> {

        if (dataPointsQuery === null || dataPointsQuery === undefined) {
            throw new Error('Required parameter dataPointsQuery was null or undefined when calling identifyModuleDescriptions1.');
        }

        let queryParameters = new HttpParams({ encoder: new HttpUrlEncodingCodec() });
        if (projectId !== undefined && projectId !== null) {
            queryParameters = queryParameters.set('projectId', projectId);
        }

        const headers = this.defaultHeaders;

        return this.httpClient.post<string[]>(`${this.basePath}/api/v2/graphql`,
            dataPointsQuery,
            {
                params: queryParameters,
                headers,
                observe,
                reportProgress
            }
        );
    }

}
