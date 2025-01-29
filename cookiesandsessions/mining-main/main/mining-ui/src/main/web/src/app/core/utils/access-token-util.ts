/** export the access token constant to be used for extracting access token from urls. */
export const ACCESS_TOKEN = 'access-token';

/**
 * Gets the search param value from the specified url.
 *
 * @param url the url to extract the search param from
 * @param searchParam the param to search for in the url
 * @returns the value of the search param
 */
export const getSearchParamValueFromURL = (url: URL, searchParam: string): string => url.searchParams.get(searchParam);

/**
 * Constructs the url from the url path and gets the search param value from the constructed url.
 *
 * @param urlPath the url path to extract the search param from
 * @param searchParam the param to search for in the url path
 * @returns the value of the search param
 */
export const getSearchParamValueFromPath = (urlPath: string, searchParam: string): string => {
    const url: URL = new URL('https://localhost/' + urlPath);
    return getSearchParamValueFromURL(url, searchParam);
};

/**
 * Returns boolean if the specified url has access-token as a param.
 *
 * @param url the url to check for access token
 * @returns true if access token is present in url path else false
 */
export const isAccessTokenPresentInPath= (url: string): boolean => !! getSearchParamValueFromPath(url, ACCESS_TOKEN);

