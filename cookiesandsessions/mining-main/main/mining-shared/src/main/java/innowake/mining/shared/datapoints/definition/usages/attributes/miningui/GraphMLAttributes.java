/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages.attributes.miningui;

/**
 * Usage attributes for {@link innowake.mining.shared.datapoints.definition.usages.Usages#MINING_UI_GRAPHML_EXPORT}.
 */
public class GraphMLAttributes {

    private GraphMLAttributes() {}

    /**
     * Determines the SQL fragment to use when querying the datapoint for the GraphML export.
     */
    public static final String SQL_FRAGMENT = "sqlFragment";
}
