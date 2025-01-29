/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;

/**
 * HTTP REST service to request the {@link JobInformation}s of multiple jobs
 * matching the provided RSQL query.
 */
public class GetJobInfos extends RestService<JobInformation[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/jobs";
	
	@Nullable
	private Map<JobInfoFieldName, Map<String, Object>> filterObject;
	
	@Nullable
	private Integer page;
	
	@Nullable
	private Integer size;
	
	@Nullable
	private List<Map<JobInfoFieldName, SortDirection>> sortObject;
		
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected GetJobInfos(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the filters for the {@link JobInformation} query.
	 * <p>The names of the filters must be present in {@link JobInfoFieldName}. The operators like {@code 'eq'}, {@code 'lt'} or {@code 'gte'} must be present
	 * in {@link FilterOperators}. Time values have to be in ISO-8601 format.</p>
	 * <p><b>Example:</b> Filter by job id in a list of job ids and filter by job status == 'SUCCESS'.</p>
	 * <pre>
	 * final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
	 * filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_IN, List.of(jobIds[0], jobIds[1])));
	 * filterObject.put(JobInfoFieldName.STATUS, Map.of(FilterOperators.OPERATOR_EQ, JobStatus.SUCCESS.name()));
	 * </pre>
	 *
	 * @param filterObject map containing the filters
	 * @return {@code this}
	 */
	public GetJobInfos setFilter(final Map<JobInfoFieldName, Map<String, Object>> filterObject) {
		this.filterObject = filterObject;
		return this;
	}

	/**
	 * Sets page number.
	 *
	 * @param page page number
	 * @return {@code this}
	 */
	public GetJobInfos setPage(final Integer page) {
		this.page = page;
		return this;
	}

	/**
	 * Sets the size of the page.
	 *
	 * @param size page of the page
	 * @return {@code this}
	 */
	public GetJobInfos setSize(final Integer size) {
		this.size = size;
		return this;
	}
	
	/**
	 * Sets the sorting for the {@link JobInformation} query.
	 * <p>The names of the sort fields must be present in {@link JobInfoFieldName}.</p>
	 * <p><b>Example:</b> Sort by finish time descending and start time ascending.</p>
	 * <pre>
	 * final List<Map<JobInfoFieldName, SortDirection>> sortObject = new ArrayList<>();
	 * sortObject.add(Map.of(JobInfoFieldName.FINISH_TIME, SortDirection.DESCENDING));
	 * sortObject.add(Map.of(JobInfoFieldName.START_TIME, SortDirection.ASCENDING));
	 * </pre>
	 *
	 * @param sortObject list of sort maps containing the sort conditions
	 * @return {@code this}
	 */
	public GetJobInfos setSortObject(final List<Map<JobInfoFieldName, SortDirection>> sortObject) {
		this.sortObject = sortObject;
		return this;
	}	


	/**
	 * Request the {@link JobInformation}s based on the provided RSQL query by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * 
	 * @return a result holding the {@link JobInformation}s if the call was successful
	 */
	@Override
	public Result<JobInformation[]> execute() throws IOException {
		validate();
		
		try {
			final URIBuilder uri = new URIBuilder(ENDPOINT);
			if (filterObject != null) {
				uri.addParameter("query", PojoMapper.jsonWriter().writeValueAsString(filterObject));
			}
			if (page != null) {
				uri.setParameter("page", page.toString());
			}
			if (size != null) {
				uri.setParameter("size", size.toString());
			}
			if (sortObject != null) {
				uri.setParameter("sortBy", PojoMapper.jsonWriter().writeValueAsString(sortObject));
			}
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<JobInformation[]>() {});
	}

}
