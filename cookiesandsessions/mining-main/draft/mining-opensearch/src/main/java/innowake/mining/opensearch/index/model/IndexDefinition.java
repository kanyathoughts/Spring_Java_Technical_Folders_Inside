package innowake.mining.opensearch.index.model;

import java.util.List;

/**
 * Defines properties describing an index to be built.
 */
public class IndexDefinition {

    private String queryName;
    private String usage;
    private String rootType;
    private List<DataPointDefinition> dataPoints;

    private String id;

    /**
     * Name of the GraphQL query that is used to retrieve the data for this index.
     * @return the query name
     */
    public String getQueryName() {
        return queryName;
    }

    public void setQueryName(final String queryName) {
        this.queryName = queryName;
    }

    /**
     * Only index data points with the given usage (additionally, the data point must be marked as filterable and/or sortable)
     * @return the usage name
     */
    public String getUsage() {
        return usage;
    }

    public void setUsage(final String usage) {
        this.usage = usage;
    }

    /**
     * The name of root type returned by the GraphQL query (TODO: could be discovered automatically from the GraphQL schema)
     * @return the name of the root type
     */
    public String getRootType() {
        return rootType;
    }

    public void setRootType(final String rootType) {
        this.rootType = rootType;
    }

    /**
     * The path of the id datapoint, i.e. the data point that identifies each row in the index uniquely.
     * @return the path of the id datapoint
     */
    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

	
	/**
	 * FIXME: description
	 *
	 * @return FIXME: description
	 */
	public List<DataPointDefinition> getDataPoints() {
		return dataPoints;
	}

	
	/**
	 * FIXME: description
	 *
	 * @param dataPoints FIXME: description
	 */
	public void setDataPoints(List<DataPointDefinition> dataPoints) {
		this.dataPoints = dataPoints;
	}
    
    
}
