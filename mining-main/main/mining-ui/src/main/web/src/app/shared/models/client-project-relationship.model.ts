/**
 * Model class for project and client relationship.
 */
export class ClientProjectRelationship {
    /**
     * Sets the values for the ClientProjectRelationship properties
     */
    constructor(
        private clientId: number,
        private clientName: string,
        private projectId?: number,
        private projectName?: string,
        private metricsDate?: Date | string) {
    }
    /**
     * get projectId
     */
    public getProjectId(): number {
        return this.projectId;
    }
    /**
     * get projectName
     */
    public getProjectName(): string {
        return this.projectName;
    }
    /**
     * get metricsDate
     */
    public getLastScanDate(): Date | string {
        return this.metricsDate;
    }
    /**
     * get clientId
     */
    public getClientId(): number {
        return this.clientId;
    }
    /**
     * get clientName
     */
    public getClientName(): string {
        return this.clientName;
    }
}
