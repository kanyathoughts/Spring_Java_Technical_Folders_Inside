package innowake.mining.extensions.example.customtable.model;

import innowake.mining.extensions.example.customtable.CustomReportExampleExtension;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

import java.util.Date;

/**
 * Example entity for custom report
 */
@MiningDataType(name = "CustomReport")
public class CustomReport {

    @MiningDataPoint(displayName = "Id", description = "Id")
    @Usage(value = CustomReportExampleExtension.EXAMPLE_USAGE)
    private Long id;

    @MiningDataPoint(displayName = "Name", description = "Example Name Column")
    @Usage(value = CustomReportExampleExtension.EXAMPLE_GRAPHQL_USAGE, attributes = {
            @UsageAttribute(key = SearchFilterAttributes.SQL_FRAGMENT_EQ, value = "") /* required for now :-( */
    })
    @Usage(value = CustomReportExampleExtension.EXAMPLE_USAGE, attributes = {
            @UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1"),
            @UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
    })
    @Usage(value = Usages.SEARCH_FILTER, attributes = {
            @UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_TEXT),
    })
    private String name;

    @MiningDataPoint(displayName = "Text", description = "Example Text Column")
    @Usage(value = CustomReportExampleExtension.EXAMPLE_USAGE, attributes = {
            @UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2"),
            @UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
    })
    private String text;

    @MiningDataPoint(displayName = "Date", description = "Example Date Column")
    @Usage(value = CustomReportExampleExtension.EXAMPLE_USAGE, attributes = {
            @UsageAttribute(key = TableAttributes.CATEGORY, value = "Additional Data")
    })
    private Date date;

    public Long getId() {
        return id;
    }

    public void setId(final Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public String getText() {
        return text;
    }

    public void setText(final String text) {
        this.text = text;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(final Date date) {
        this.date = date;
    }
}
