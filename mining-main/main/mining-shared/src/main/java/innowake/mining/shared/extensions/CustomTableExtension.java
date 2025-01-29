package innowake.mining.shared.extensions;

/**
 * Interface for Mining extensions that provide custom tables on the web-ui.
 */
public interface CustomTableExtension extends AccessRestrictedExtension {

    /**
     * An identifier for this extension.
     * <p>
     * This identifier must be unique because it is used in the URL for the custom page
     *
     * @return a unique identifier
     */
    String getIdentifier();

    /**
     * A short name for the custom table which is shown in the menu.
     * @return a short name
     */
    String getName();

    /**
     * Short descriptive text of this extension.
     *
     * @return a short description
     */
    String getDescription();

    /**
     * Name of the GraphQL root query that is used to get the data for the table.
     * @return the name of the GraphQL query
     */
    String getQueryName();

    /**
     * The mining data point (and GraphQL) schema type that is returned by the query. Used to retrieve the applicable data points.
     *
     * @return name of the root type of the query
     */
    String getRootTypeName();

    /**
     * The usage for data points that are to be displayed on the table.
     *
     * @return the data point usage
     */
    String getUsage();
}
