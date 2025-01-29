package innowake.mining.opensearch.index.model;

import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;

import java.util.List;

public class IndexStatus {

    public enum Status {
        INDEXING,
        READY
    }

    private final String indexName;
    private final Long projectId;
    private Status status;
    private double indexProgress;
    private final IndexDefinition indexDefinition;
    private final List<MiningDataPointDefinitionWithPath> dataPoints;

    public IndexStatus(final String indexName, final Long projectId, final IndexDefinition indexDefinition, final List<MiningDataPointDefinitionWithPath> dataPoints) {
        this.indexName = indexName;
        this.indexDefinition = indexDefinition;
        this.projectId = projectId;
        this.dataPoints = dataPoints;
    }

    public String getIndexName() {
        return indexName;
    }

    public Long getProjectId() {
        return projectId;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(final Status status) {
        this.status = status;
    }

    public double getIndexProgress() {
        return indexProgress;
    }

    public void setIndexProgress(final double indexProgress) {
        this.indexProgress = indexProgress;
    }

    public IndexDefinition getIndexDefinition() {
        return indexDefinition;
    }

    public List<MiningDataPointDefinitionWithPath> getDataPoints() {
        return dataPoints;
    }
}
