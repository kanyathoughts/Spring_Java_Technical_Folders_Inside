/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.ModelArtifactService;
import innowake.mining.data.access.postgres.ModelArtifactPgDao;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Access to module related data.
 * <p>This service should be used for the discovery only when instances of {@code ModelArtifact} or {@code ModelStatement} are required. For all other cases
 * use the {@link ModuleService} instead.</p>
 */
@Service
public class ModelArtifactServiceImpl implements ModelArtifactService {

	private final ModelArtifactPgDao modelArtifactDao;

	@Autowired
	public ModelArtifactServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		modelArtifactDao = new ModelArtifactPgDao(jdbcTemplate);
	}

	@Override
	public List<LazyModelArtifact> find(final BuildingConsumer<ModelArtifactInquiryBuilder> builder) {
		return modelArtifactDao.find(builder);
	}

	@Override
	public Optional<LazyModelArtifact> findAny(final BuildingConsumer<ModelArtifactInquiryBuilder> builder) {
		 final List<LazyModelArtifact> matches = modelArtifactDao.find(builder);
		 return matches.isEmpty() ? Optional.empty() : Optional.of(matches.get(0));
	}
}
