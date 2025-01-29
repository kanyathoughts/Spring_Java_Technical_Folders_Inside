package innowake.mining.server.dao;

import innowake.mining.data.access.postgres.ErrorMarkerPgDao;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.test.context.support.WithMockUser;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for the {@link ErrorMarkerPgDao}.
 */
@WithMockUser
class ErrorMarkerPgDaoTest extends DatabaseRelatedTest {

	private final ModuleService moduleService;
	private final ErrorMarkerPgDao errorMarkerPgDao;

	@Autowired
	public ErrorMarkerPgDaoTest(@Qualifier("postgres") final JdbcTemplate jdbcTemplate, final ModuleService moduleService) {
		this.moduleService = moduleService;
		this.errorMarkerPgDao = new ErrorMarkerPgDao(jdbcTemplate);
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

	@Test
	void testOfModulesHavingMixedCombinationsOfEntityIds() {
		final var modules = moduleService.findModules(b -> b.ofProject(EntityId.of(3L)).withNames(List.of("DeleteTestSD1", "DeleteTestSD2", "DeleteTestSD3")))
				.stream()
				.map(ModulePojo::identity)
				.toList();
		assertEquals(3, modules.size());

		/* find by module identity with different combination of EntityId having UIDs and NIDs */
		final var errorMarkers = errorMarkerPgDao.find(b -> b.ofProject(EntityId.of(3L)).ofModules(
				List.of(EntityId.of(modules.get(0).getNid()),									/* EntityId with only NID */
						EntityId.of(modules.get(1).getUid()),									/* EntityId with only UID */
						EntityId.of(modules.get(2).getUid(), modules.get(2).getNid()))));		/* EntityId with both UID and NID */
		assertEquals(4, errorMarkers.size());
	}

}
