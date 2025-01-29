/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;

/**
 * HTTP REST service for getting HotSpots.
 */
public class GetHotSpots extends ProjectIdService<GetHotSpots, HotSpot[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/hotspots/%s/%s";
	@Nullable
	private FilterType hotSpotType;
	private Integer limit = Integer.valueOf(10);

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	GetHotSpots(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all hotspots of a given project by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all {@linkplain HotSpot hotSpots} on success
	 */
	@Override
	public Result<HotSpot[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), hotSpotType, limit));
		return execute(httpGet(), new TypeReference<HotSpot[]>() {});
	}

	/**
	 * Sets the hotspot type.
	 *
	 * @param hotSpotType the type of hotspot
	 * @return the reference of current object of type {@link GetHotSpots}
	 */
	public GetHotSpots setHotSpotType(final FilterType hotSpotType) {
		this.hotSpotType = Assert.assertNotNull(hotSpotType);
		return this;
	}

	/**
	 * Sets the limit for number of hotspots to be fetched default 10.
	 * 
	 * @param limit the number of hotspot to be fetched default being 10
	 * @return the reference of current object of type {@link GetHotSpots}
	 */
	public GetHotSpots setLimit(final int limit) {
		Assert.assertTrue(limit > 0);
		this.limit = Integer.valueOf(limit);
		return this;
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (hotSpotType == null) {
			throw new IllegalStateException("Hotspotype must be set.");
		}
		if (limit.intValue() < 1) {
			throw new IllegalStateException("limit must be set greater than 0.");
		}
	}
}
