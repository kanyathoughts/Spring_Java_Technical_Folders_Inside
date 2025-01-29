/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.hashing.CityHash;
import db.migration.model.legacy.SourceObject;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Migration script for migrating source attachment to source object
 */
@SuppressWarnings({"removal", "deprecation"})
public class V1_2_134__MigrateSourceAttachment_To_SourceObject extends BaseJavaMigration{
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));
		
		final int sourceObjectWithExistingContentHashCount = jdbcTemplate
				.update("INSERT INTO SourceObject FROM SELECT sourceAttachmentLink.content as content, "
						+ "contentHash, 1 as contentRevision, sequence('SourceObject_Sequence').next() as id, "
						+ "1 as metaDataRevision, name, path, projectLink, objectTypeLink.technologyLink as technologyLink, "
						+ "objectTypeLink.typeLink as typeLink FROM Module "
						+ "WHERE sourceAttachmentLink.@class='SourceAttachment' and path IS NOT NULL and contentHash IS NOT NULL");
		
		LOG.info("Number of source Objects created from module whose contentHash IS NOT NULL: " + sourceObjectWithExistingContentHashCount);
		
		final List<SourceObject> sourceObjectList = jdbcTemplate.query("SELECT projectLink.id, name, path, objectTypeLink.technologyLink.name,"
				+ "objectTypeLink.typeLink.name, metaDataRevision, contentRevision, sourceAttachmentLink.content as content,"
				+ "contentHash FROM Module WHERE sourceAttachmentLink.@class='SourceAttachment' "
	            + "and path IS NOT NULL and contentHash IS NULL", new SourceObjectRowMapper());

		LOG.info("Number of source Objects to be created from the module whose contentHash IS NULL along with creation of this contentHash in SourceObject: "
				+ sourceObjectList.size());
		
		sourceObjectList.forEach(sourceObject -> jdbcTemplate.update("INSERT INTO SourceObject SET projectLink=(SELECT FROM Project WHERE id=?), name=?,"
				+ "path=?, technologyLink=(SELECT FROM TechnologyEnum WHERE name=?), typeLink = (SELECT FROM TypeEnum WHERE name=?), "
				+ "metaDataRevision=1, contentRevision=1, content=?, contentHash=?, id= sequence('SourceObject_Sequence').next()", sourceObject.getProjectId(),
				sourceObject.getName(), sourceObject.getPath(), sourceObject.getTechnology().toString(), sourceObject.getType().toString(),
				sourceObject.getContent(), sourceObject.getContentHash()));
		
		LOG.info("Source Objects from the module whose contentHash IS NULL is created successfully along with the creation of contentHash in SourceObject");
		LOG.warn("Source objects are not created for the Source attachment whose path IS NULL in the Module");
	}

	private class SourceObjectRowMapper implements RowMapper<SourceObject> {
		@Override
		@Nullable
		public SourceObject mapRow(final ResultSet resultSet, final int rowNum) throws SQLException {
			final String contentHash = CityHash.cityHash128Hex(StringUtils.trimToEmpty(resultSet.getString("content")));
			return new SourceObject(
					(Long) resultSet.getObject("projectLink.id"),
					resultSet.getString("name"),
					resultSet.getString("path"),
					Technology.fromName(resultSet.getString("objectTypeLink.technologyLink.name")),
					Type.fromName(resultSet.getString("objectTypeLink.typeLink.name")),
					(Long) resultSet.getObject("metaDataRevision"),
					(Long) resultSet.getObject("contentRevision"),
					resultSet.getString("content"),
					contentHash);
		}
	}
}
