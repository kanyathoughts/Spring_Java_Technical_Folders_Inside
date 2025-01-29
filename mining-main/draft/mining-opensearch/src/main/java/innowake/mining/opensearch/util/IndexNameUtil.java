package innowake.mining.opensearch.util;

import org.apache.commons.lang3.tuple.Pair;

public final class IndexNameUtil {

    private static final String PROJECT_DELIMITER = "_project_";

    private IndexNameUtil() {
        /* static utility class */
    }

    public static String buildIndexName(final String indexName, final Long projectId) {
        return indexName + PROJECT_DELIMITER + projectId;
    }

    public static Pair<String, Long> parseIndexName(final String indexName) {
        final String[] split = indexName.split(PROJECT_DELIMITER);
        if (split.length != 2) {
            throw new IllegalArgumentException("invalid index name: " + indexName);
        }

        return Pair.of(split[0], Long.parseLong(split[1]));
    }
}
