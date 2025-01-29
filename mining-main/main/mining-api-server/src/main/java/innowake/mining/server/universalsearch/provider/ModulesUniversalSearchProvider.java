/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.universalsearch.provider;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.universalsearch.UniversalSearchLink;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Provider class to search the query among the modules.
 */
@Component
public class ModulesUniversalSearchProvider implements UniversalSearchProvider {

	private final ModuleService moduleService;
	
	/**
	 * Constructor.
	 * 
	 * @param moduleService the {@link ModuleService}
	 */
	@Autowired
	public ModulesUniversalSearchProvider(final ModuleService moduleService) {
		this.moduleService = moduleService;
	}

	@Override
	public String getIdentifier() {
		return "module-name-path";
	}

	@Override
	public List<UniversalSearchResult> query(final EntityId projectId, final String name) {
		return moduleService.findModules(new Pagination(10), q -> q.ofProject(projectId).withName('%' + name.trim() + '%', true)).getContent().stream()
				.map(module -> new UniversalSearchResult(getIdentifier(), UniversalSearchResult.RANK_DEFAULT, module.getName(), module.getPath().orElse(null), 
						module.getDescription().orElse(null), "Module", List.of(UniversalSearchLink.forModuleDetailsPage(module.getId()))))
				.collect(Collectors.toList());
	}
}
