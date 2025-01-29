package innowake.mining.server.genai.requestresponse;

import innowake.mining.server.properties.GenericConfigProperties;

/**
 * Interface to customize the request for each service
 */
public interface GenAiRequest {

	/**
	 * generating the url path for each gen-ai request
	 * @param configProperties contains all configuration of gen ai
	 * @return url path
	 */
	String getUrlPath(GenericConfigProperties configProperties);

}
