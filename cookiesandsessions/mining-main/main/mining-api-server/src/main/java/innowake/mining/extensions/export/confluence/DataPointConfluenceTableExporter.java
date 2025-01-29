package innowake.mining.extensions.export.confluence;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Uses Mining Datapoints to export tables in Confluence Markup format.
 * Module columns are replaced by links to the Modules. The first row is a header row.
 * 
 *  Example table:
 * ||Module Name||Technology||Type||Last Scan||
 * |[MMRS710D|http://127.0.0.1:8080/#/project-5/module-QGV0A3X8WRcrg1sFrrR6l/details/overview]|COBOL|COPYBOOK|2022-03-03T13:21:36.922Z|
 * |[MMRS71B1|http://127.0.0.1:8080/#/project-5/module-3WJrbt8Nw8dUjxbYRxT0Vc/details/overview]|COBOL|PROGRAM|2022-03-03T13:21:36.922Z|
 * |[MMRS711S|http://127.0.0.1:8080/#/project-5/module-cX5saIJwTVQGqLNApFfz5/details/overview]|JCL|JOB|2022-03-03T13:21:36.922Z|
 * 
 * @author jsimianer
 */
@Service
public class DataPointConfluenceTableExporter implements MiningJobExtension<FileSystemResult> {
	
	@Override
	public NatureType getRequiredNature() {
		return MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return VIEWER;
	}

	@Override
	public String getDescription() {
		return "Exports Mining Tables to Confluence Markup using Mining Datapoints";
	}

	@Override
	public String getIdentifier() {
		return "datapoint-confluence-table";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new DataPointConfluenceTableExporterJob(projectId, parameters, ServletUriComponentsBuilder.fromCurrentContextPath().build().toUriString(), 
				SecurityContextHolder.getContext());
	}

}
