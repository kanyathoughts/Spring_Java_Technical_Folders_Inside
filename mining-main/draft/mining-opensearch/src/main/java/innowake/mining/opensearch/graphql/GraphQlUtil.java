package innowake.mining.opensearch.graphql;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.definition.AliasDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import org.apache.commons.lang.StringUtils;
import org.springframework.graphql.support.DefaultExecutionGraphQlRequest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class GraphQlUtil {

    private GraphQlUtil() {
        /* static utility class */
    }

    /**
     * Internal class for turning list of datapoints into GraphQL query.
     */
    private static class Selection {
        private final String field;
        private final Map<String, Object> parameters = new HashMap<>();
        private final List<Selection> subSelections = new ArrayList<>();

        @Nullable
        private final AliasDefinition aliasDefinition;

        public Selection(final String field) {
            this.field = field;
            this.aliasDefinition = null;
        }

        public Selection(final String field, @Nullable final AliasDefinition aliasDefinition) {
            this.field = field;
            this.aliasDefinition = aliasDefinition;
        }
    }

    /**
     * Class encapsulating a parameter variable. This is used internally for building the GraphQL query and will be rendered as {@code $name}.
     */
    private static class ParameterVariable {
        private final String name;

        public ParameterVariable(final String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return "$" + name;
        }
    }

    /**
     * Internal class for managing parameters passed to data points.
     * <p>
     * Unfortunately the parameter handling is quite complex. Let's say the {@code parameters} map
     * passed to the constructor contains the parameter {@code foo.bar.baz: "a string value"} and
     * {@code queryName} is "modules". In that case the {@link #getGraphQlQuery(String, QueryParameters, List)}
     * method would need to build the following construct:
     * <pre>
     *     query($foo_bar_baz: String!) {
     *         modules {
     *             foo {
     *                 bar(baz: $foo_bar_baz)
     *             }
     *         }
     *     }
     * </pre>
     * So there are a couple of things that need to be taken care of:
     * <ul>
     *     <li>we need to build a variable name for the parameter ({@code $foo_bar_baz} in the example)</li>
     *     <li>we need to determine the parameter type, because that is required for some reason</li>
     *     <li>we need to parse the "parameter path" ({@code foo.bar}) and parameter name {@code baz} and put it at the correct location in the query</li>
     * </ul>
     */
    public static class QueryParameters {
        private final Map<String, Object> parameters;
        private final Map<String, String> types;

        /**
         * Create new QueryParameters object.
         *
         * @param parameters map of parameters to be passed to the query
         * @param types the parameter types
         */
        public QueryParameters(final Map<String, Object> parameters, final Map<String, String> types) {
            this.parameters = parameters;
            this.types = types;
        }

        /**
         * Returns all parameter keys that are in the parameters map that was passed to the constructor.
         * @return the set of parameters
         */
        private Set<String> getKeys() {
            return parameters.keySet();
        }

        /**
         * Returns the parameter keys that are applicable to the data point at {@code path}.
         *
         * @param path the data point path for which to return the parameter keys
         * @return a (possibly empty) list of parameter keys
         */
        private List<String> getKeysForPath(final String path) {
            final List<String> pathSegments = StringUtils.isEmpty(path) ? Collections.emptyList() : Arrays.asList(path.split("\\."));
            return parameters.keySet().stream()
                    .filter(key -> {
                        final List<String> segments = Arrays.asList(key.split("\\."));
                        return (segments.size() <= 1 ? Collections.emptyList() : segments.subList(0, segments.size() - 1)).equals(pathSegments);
                    })
                    .collect(Collectors.toList());
        }

        /**
         * Gets the field name of the parameter with the given key. The field name is the last segment of the key.
         * <p>
         * Example: calling the method with key {@code "foo.bar.baz"} returns "baz".
         * @param key the parameter key
         * @return the field name portion of the key
         */
        private String getFieldName(final String key) {
            final String[] pathSegments = key.split("\\.");
            return pathSegments[pathSegments.length - 1];
        }

        /**
         * Returns a {@link ParameterVariable} object that can be used as a placeholder for the parameter inside the query.
         * <p>
         * Example: calling the method with key {@code "foo.bar.baz"} will return a {@code ParameterVariable} with name {@code $foo_bar_baz}.
         * @param key the parameter key
         * @return a {@link ParameterVariable} object that can be used as a placeholder for the key
         */
        private ParameterVariable getVariable(final String key) {
            return new ParameterVariable(key.replace('.', '_'));
        }

        /**
         * Gets the type of the given parameter as it needs to be represented in the schema.
         *
         * @return the type of the parameter
         */
        private String getType(final String key) {
            return types.get(key);
        }

        /**
         * Returns a map of the query parameters that can be passed to
         * {@link DefaultExecutionGraphQlRequest#DefaultExecutionGraphQlRequest(String, String, Map, Map, String, Locale)}.
         * This is the same {@code parameters} map that passed to the constructor,
         * but the parameter names are replaced with the variable names used inside the query.
         * <p>
         * Example: if the original parameter map contained key {@code "foo.bar.baz"} the returned map will contain key {@code "foo_bar_baz"}.
         * @return parameter map that can be used as request input for
         * {@link DefaultExecutionGraphQlRequest#DefaultExecutionGraphQlRequest(String, String, Map, Map, String, Locale)}
         */
        public Map<String, Object> toParameterMap() {
            return parameters.entrySet().stream().collect(Collectors.toMap(entry -> entry.getKey().replace(".", "_"), entry -> entry.getValue()));
        }
    }

    public static String getGraphQlQuery(final String queryName, final QueryParameters parameters, final List<MiningDataPointDefinitionWithPath> dataPoints) {
        final Selection root = new Selection(queryName);
        root.parameters.putAll(parameters.getKeysForPath("").stream().collect(Collectors.toMap(parameters::getFieldName, parameters::getVariable)));

        for (final MiningDataPointDefinitionWithPath dataPoint : dataPoints) {
            final List<String> pathSegments = Arrays.asList(dataPoint.getPath().split("\\."));
            final Selection selection = new Selection(dataPoint.getName(), dataPoint.getAliasFor());
            selection.parameters.putAll(parameters.getKeysForPath(dataPoint.getPath()).stream().collect(Collectors.toMap(parameters::getFieldName, parameters::getVariable)));
            putSelectionAtPath(root, selection, Collections.emptyList(), pathSegments, parameters);
        }

        final String query = selectionToString(root);

        if (parameters.getKeys().isEmpty()) {
            return "query {" + query + "}";
        } else {
            return "query(" + parameterNamesToString(parameters) + ") {" + query + "}";
        }

    }

    private static void putSelectionAtPath(final Selection parent, final Selection selection, final List<String> pathFromRoot, final List<String> path, final QueryParameters parameters) {
        final AliasDefinition aliasDefinition = selection.aliasDefinition;
        /*
         * If there is only 1 remaining path segment: Put the selection at the current path. The remaining path segment is just the name of the field,
         * but there are no further sub-selections.
         *
         * If there are more path segments and the selection has an alias definition and the remaining path segments equal
         * the sub-selection of the alias definition: Put the selection at the current path, the remaining path segments for the alias
         * sub-selection will be added later by aliasToString().
         */
        if (path.size() == 1 || aliasDefinition != null && selection.field.equals(path.get(0))
                && aliasDefinition.getSubSelection().equals(String.join(".", path.subList(1, path.size())))) {
            parent.subSelections.add(selection);
        } else {
            final List<String> newPathFromRoot = new ArrayList<>(pathFromRoot);
            newPathFromRoot.add(path.get(0));
            for (final Selection subSelection : parent.subSelections) {
                if (subSelection.field.equals(path.get(0))) {
                    putSelectionAtPath(subSelection, selection, newPathFromRoot, path.subList(1, path.size()), parameters);
                    return;
                }
            }
            final Selection subSelection = new Selection(path.get(0));
            subSelection.parameters.putAll(parameters.getKeysForPath(String.join(".", newPathFromRoot)).stream().collect(Collectors.toMap(parameters::getFieldName, parameters::getVariable)));
            parent.subSelections.add(subSelection);
            putSelectionAtPath(subSelection, selection, newPathFromRoot, path.subList(1, path.size()), parameters);
        }
    }

    private static String selectionToString(final Selection selection) {
        final StringBuilder sb = new StringBuilder();
        if (selection.aliasDefinition != null) {
            sb.append(aliasToString(selection.field, selection.aliasDefinition));
        } else {
            sb.append(selection.field);
            sb.append(parametersToString(selection.parameters));
            if ( ! selection.subSelections.isEmpty()) {
                sb.append(" { ");
                sb.append(selection.subSelections.stream().map(GraphQlUtil::selectionToString).collect(Collectors.joining(" ")));
                sb.append(" } ");
            }
        }
        return sb.toString();
    }

    private static String aliasToString(final String field, final AliasDefinition aliasDefinition) {
        final StringBuilder sb = new StringBuilder();

        sb.append(field);
        sb.append(": ");

        sb.append(aliasDefinition.getAliasFor());
        if ( ! aliasDefinition.getParameters().isEmpty()) {
            sb.append("(");
            sb.append(String.join(", ", aliasDefinition.getParameters()));
            sb.append(")");
        }

        final List<String> subSelectionPath = Arrays.asList(aliasDefinition.getSubSelection().split("\\."));
        if ( ! subSelectionPath.isEmpty() && ! subSelectionPath.get(0).isEmpty()) {
            final int openBraces = subSelectionPath.size();
            sb.append(" { ");
            sb.append(String.join(" { ", subSelectionPath));
            for (int i = 0; i < openBraces; i++) {
                sb.append(" }");
            }
        }

        return sb.toString();
    }

    private static String parametersToString(final Map<String, Object> parameters) {
        if (parameters.isEmpty()) {
            return "";
        }

        final List<String> paramStrings = new ArrayList<>();
        for (final Map.Entry<String, Object> entry : parameters.entrySet()) {
            if (entry.getValue() instanceof String) {
                /* needs quote */
                paramStrings.add(entry.getKey() + ": \"" + entry.getValue() + "\"");
            } else {
                paramStrings.add(entry.getKey() + ": " + entry.getValue());
            }
        }
        return "(" + String.join(", ", paramStrings) + ")";
    }

    private static String parameterNamesToString(final QueryParameters parameters) {
        return parameters.getKeys().stream()
                .map(key -> parameters.getVariable(key) + ":" + parameters.getType(key))
                .collect(Collectors.joining(","));
    }
}
