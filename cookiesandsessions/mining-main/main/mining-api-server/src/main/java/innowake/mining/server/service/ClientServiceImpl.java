/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.access.postgres.ClientPgDao;
import innowake.mining.data.access.postgres.ProjectPgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.event.MarkedForDeletionEvent;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.BinaryAttachmentPojo;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Central point for accessing and modifying Client entities.
 */
@Service
public class ClientServiceImpl implements ClientService {
	
	private static final Logger LOG = LoggerFactory.getLogger(ClientService.class);
	
	private final ClientPgDao clientDao;
	private final ProjectPgDao projectDao;
	
	@Autowired
	private AuthorizationManagementService authorizationManagementService;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	public ClientServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		clientDao = new ClientPgDao(jdbcTemplate);
		projectDao = new ProjectPgDao(jdbcTemplate);
	}
	
	private void createAuthorization(final Long nid) {
		authorizationManagementService.createClientAttributes(nid);
	}
	
	@Transactional("postgres")
	@Override
	public ClientPojo create(final ClientPojoPrototype client) {
		final ClientPojo newClient = get(clientDao.put(client, true), false);
		createAuthorization(newClient.getId());
		return newClient;
	}
	
	@Transactional("postgres")
	@Override
	public ClientPojo create(final String name) {
		return create(new ClientPojoPrototype().setName(name));
	}
	
	@Override
	public void update(final ClientPojoPrototype client) {
		clientDao.put(client, false);
	}
	
	@Override
	public Long count(@Nullable final Boolean markedForDeletion) {
		return clientDao.count(markedForDeletion);
	}
	
	@Override
	public List<ClientPojo> find(final BuildingConsumer<ClientInquiryBuilder> builder) {
		return clientDao.find(builder);
	}
	
	@Override
	public Paged<ClientPojo> find(final Pagination paging, final BuildingConsumer<ClientInquiryBuilder> builder) {
		return clientDao.find(paging, builder);
	}
	
	@Override
	public Optional<ClientPojo> find(final UUID uid) {
		return clientDao.find(uid);
	}
	
	@Override
	public ClientPojo get(final UUID uid) {
		return clientDao.find(uid)
				.orElseThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, uid.toString()));
	}
	
	@Override
	public Optional<ClientPojo> find(final EntityId clientId, final boolean onlyValid) {
		return clientDao.find(clientId, onlyValid);
	}
	
	@Override
	public ClientPojo get(final EntityId clientId, final boolean onlyValid) {
		return clientDao.find(clientId, onlyValid)
				.orElseThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, clientId.toString()));
	}
	
	@Override
	public BinaryAttachmentPojo getLogo(final EntityId clientId) {
		return clientDao.fetchLogo(clientId)
				.orElseThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, clientId.toString()));
	}
	
	@Override
	public void setLogo(final EntityId clientId, @Nullable final BinaryAttachmentPojo logo) {
		clientDao.updateLogo(clientId, logo);
	}
	
	private void markedForDeletion(final EntityId clientId, final boolean triggerJob) {
		projectDao.markForDeletionByClient(clientId);
		try {
			authorizationManagementService.deleteClientAttributes(clientId.getNid());
		} catch (final Exception e) {
			LOG.error(() -> String.format("Deletion of all client roles and groups for specified Client %s failed", clientId.toString()), e);
		}
		if (triggerJob) {
			eventPublisher.publishEvent(new MarkedForDeletionEvent());
		}
	}
	
	@Override
	public EntityId markForDeletion(final EntityId clientId) {
		final ClientPojo client = get(clientId, true);
		clientDao.markForDeletion(client.identity());
		markedForDeletion(client.identity(), true);
		return client.identity();
	}
	
	@Override
	public void deleteDirectly(final EntityId clientId) {
		final ClientPojo client = get(clientId, false);
		if (! client.isMarkedDeleted()) {
			clientDao.markForDeletion(EntityId.of(client.getUid()));
			markedForDeletion(clientId, false);
		}
		clientDao.delete(clientId);
	}
	
	@Override
	public void deleteClients() throws Exception {
		LOG.debug("Deleting all clients marked for deletion.");
		
		final List<ClientPojo> clients = clientDao.find(q -> q.withMarkedForDeletion(true));
		LOG.debug("Found {} clients to be deleted.", Integer.valueOf(clients.size()));
		
		for (final ClientPojo client : clients) {
			final List<ProjectPojo> projects = projectDao.find(q -> q.ofClient(client.identity()));
			if (projects.isEmpty()) {
				try {
					LOG.debug("Deleting client '{}' (ID: {})", client.getName(), client.getId());
					clientDao.delete(client.identity());
					LOG.info("Deleted client '{}' (ID: {})", client.getName(), client.getId());
				} catch (final Exception e) {
					LOG.debug(() -> String.format("Error while deleting client '%s' (ID: %d)", client.getName(), client.getId()), e);
					throw e;
				}
			} else {
				LOG.info("Skipping deletion: Found %d projects for client '{}' (ID: {})", Integer.valueOf(projects.size()), client.getName(), client.getId());
			}
		}
		
		LOG.debug(() -> "All clients deleted.");
	}
	
}
