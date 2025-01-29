package innowake.mining.extensions.example.customtable;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.extensions.example.customtable.model.CustomReport;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.stereotype.Controller;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * Controller providing the GraphQL query that supplies the data for the custom table.
 */
@Controller
public class CustomReportGraphQlController {

    @MiningQueryMapping
    @Role(RoleType.VIEWER)
    @Nature(NatureType.MINING)
    public Page<CustomReport> customReportData(@Argument final Long projectId,
                                               @Argument @Nullable final Integer page,
                                               @Argument @Nullable final Integer size,
                                               @Argument @Nullable final List<String> sortBy,
                                               @Argument @Nullable final String filter,
                                               @Argument(name = "sortObject") @Nullable final List<Map<String, String>> sortObject,
                                               @Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject) {

        final List<CustomReport> sampleData = new ArrayList<>();

        final CustomReport sampleReport = new CustomReport();
        sampleReport.setId(1L);
        sampleReport.setName("Hello World");
        sampleReport.setText("Some example text for the text column");
        sampleReport.setDate(Date.from(Instant.now()));
        sampleData.add(sampleReport);

        final CustomReport sampleReport2 = new CustomReport();
        sampleReport.setId(2L);
        sampleReport2.setName("Second text");
        sampleReport2.setText("Another row of data");
        sampleReport2.setDate(Date.from(Instant.now()));
        sampleData.add(sampleReport2);

        return new PageImpl<>(sampleData);
    }
}
