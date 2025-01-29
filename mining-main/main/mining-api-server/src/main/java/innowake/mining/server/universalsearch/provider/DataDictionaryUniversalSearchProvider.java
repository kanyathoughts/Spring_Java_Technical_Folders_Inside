/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.universalsearch.provider;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.universalsearch.UniversalSearchLink;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Provider class to search the query among the data dictionaries.
 */
@Component
public class DataDictionaryUniversalSearchProvider implements UniversalSearchProvider {
	
	private static final Logger LOG = LoggerFactory.getLogger(DataDictionaryUniversalSearchProvider.class);

	private final ModuleService moduleService;
	private final DataDictionaryService dataDictionaryService;
	
	@Autowired
	public DataDictionaryUniversalSearchProvider(final ModuleService moduleSevice, final DataDictionaryService dataDictionaryService) {
		this.moduleService = moduleSevice;
		this.dataDictionaryService = dataDictionaryService;
	}

	@Override
	public String getIdentifier() {
		return "data-dictionary";
	}

	@Override
	public List<UniversalSearchResult> query(final EntityId projectId, final String name) {
		return dataDictionaryService.find(new Pagination(10), q -> q.ofModuleProject(projectId).withName('%' + name.trim() + '%')).getContent().stream()
				.map(this::buildSearchResult).filter(Objects::nonNull).collect(Collectors.toList());
	}

	@Nullable
	private UniversalSearchResult buildSearchResult(final DataDictionaryPojo dataDictionary) {
		try {
			final String name = dataDictionary.getName();
			final var module = moduleService.getModuleLightweight(dataDictionary.getModule());
			final String path = module.getPath();
			final String description = dataDictionary.getDescription();
			final Long id = module.getId();
			final var moduleLocation = dataDictionary.getLocation()
					.orElseThrow(() -> new IllegalArgumentException("Data dictionary " + name + " has no module location."));
			return new UniversalSearchResult(getIdentifier(), UniversalSearchResult.RANK_DEFAULT, name, path, description, "Data Dictionary",
					List.of(UniversalSearchLink.forCodeViewer(id, moduleLocation)));
		} catch(final Exception ex) {
			LOG.error(ex.getMessage());
			return null;
		}
	}

}
