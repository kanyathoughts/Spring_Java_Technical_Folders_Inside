package innowake.mining.extensions.example.customtable;

import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import innowake.mining.shared.extensions.CustomTableExtension;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.stereotype.Component;

/**
 * Example extension that provides a custom table on the mining-ui.
 */
@Component
public class CustomReportExampleExtension implements CustomTableExtension, MiningDataPointSource {

    public static final String EXAMPLE_USAGE = "extensions.custom-report-example";
    public static final String GRAPHQL_QUERY_NAME = "customReportData";
    public static final String EXAMPLE_GRAPHQL_USAGE = Usages.GRAPHQL_QUERY_PREFIX + GRAPHQL_QUERY_NAME;

    @Override
    public NatureType getRequiredNature() {
        return NatureType.MINING;
    }

    @Override
    public RoleType getRequiredRole() {
        return RoleType.VIEWER;
    }

    @Override
    public String getIdentifier() {
        return "custom-report-example";
    }

    @Override
    public String getName() {
        return "Custom Report Example";
    }

    @Override
    public String getDescription() {
        return "Example Extension for a custom report, i.e. displaying a custom customizable table";
    }

    @Override
    public String getQueryName() {
        return GRAPHQL_QUERY_NAME;
    }

    @Override
    public String getRootTypeName() {
        return "PAGE_CustomReport";
    }

    @Override
    public String getUsage() {
        return EXAMPLE_USAGE;
    }

    @Override
    public void provideDataPoints(final MiningDataPointBuilder builder) {
        builder.defineDataPointsFromSchemaMappingAnnotations(CustomReportGraphQlController.class);

        /* currently, we must define a fake data point that is sortable and filterable or the GraphQL controller doesn't work
         * this is a bug */
        builder.defineDataPoint("CustomReport", "fakeDataPoint")
                .type(String.class)
                .withUsage(EXAMPLE_GRAPHQL_USAGE)
                .withUsageAttribute(EXAMPLE_GRAPHQL_USAGE, SearchFilterAttributes.SQL_FRAGMENT_EQ, "")
                .withUsageAttribute(EXAMPLE_GRAPHQL_USAGE, SortByAttributes.SQL_FRAGMENT_ORDER_BY, "")
                .add();
    }
}
