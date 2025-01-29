/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.BinaryAttachmentPojo;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Postgres specific access methods for the {@code client} entity.
 */
public class ClientPgDao extends PgDao {
	
	/**
	 * Creates a new Client entity access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public ClientPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Consumer for applying the given {@code clientId} to a {@link QueryBuilder}.
	 * 
	 * <p>The given {@code clientId} must contain either a {@link UUID} or a numeric id, or both</p>
	 * <p>If the {@code clientId} contains a {@link UUID} then this {@link UUID} is used. Otherwise a sub-query is added to query for the client {@link UUID}
	 * by the numeric id in {@code clientId}.</p>
	 *
	 * @param clientId {@link EntityId} of a client
	 * @return
	 */
	static Consumer<QueryBuilder> referenceUidOrNid(@Nullable final EntityId clientId) {
		if (clientId == null || clientId == EntityId.VOID || clientId.value() == null) {
			return q -> q.append("?").addArg(null);
		}

		return q -> q.appendId(clientId, "?", "(SELECT uid FROM client WHERE nid = ?)");
	}

	/**
	 * Query builder for performing queries on {@code client} entities.
	 */
	public class ClientQueryBuilder implements ClientService.ClientInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		
		protected Paged.Builder<ClientPojo> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("client.uid", SortDirection.ASCENDING));
			}
			return query("SELECT uid, nid, name, logo is not null, to_be_deleted, custom_properties FROM client")
				.with(filters::build)
				.with(order::build)
				.toPageable(paging, (rs, row) -> new ClientPojo(
					(UUID) rs.getObject(1),												/* client uid */
					new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(6))),		/* client custom_properties */
					Long.valueOf(rs.getLong(2)),										/* client nid */
					rs.getString(3),													/* client name */
					Boolean.valueOf(rs.getBoolean(4)),									/* client has logo as boolean */
					Boolean.valueOf(rs.getBoolean(5))									/* client to_be_deleted */
				));
		}
		
		protected ClientQueryBuilder byUid(final UUID uid) {
			filters.accept(q -> q.append("uid = ?", uid));
			return this;
		}
		
		protected ClientQueryBuilder byNid(final Long nid) {
			filters.accept(q -> q.append("nid = ?", nid));
			return this;
		}
		
		@Override
		public ClientQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("name ILIKE ?", name));
			return this;
		}
		
		@Override
		public ClientQueryBuilder withIdAbove(final Long nid) {
			filters.accept(q -> q.append("nid > ?", nid));
			return this;
		}
		
		@Override
		public ClientQueryBuilder withIds(final Collection<Long> nids) {
			filters.accept(q -> q.append("nid = any(?)").addArg(PgType.LONG, nids));
			return this;
		}
		
		@Override
		public ClientQueryBuilder withMarkedForDeletion(final boolean toBeDeleted) {
			filters.accept(q -> q.append("to_be_deleted = ?", Boolean.valueOf(toBeDeleted)));
			return this;
		}
		
		@Override
		public ClientQueryBuilder sortNid(final SortDirection direction) {
			order.accept(q -> q.appendOrder("nid", direction));
			return this;
		}
		
		@Override
		public ClientQueryBuilder sortName(final SortDirection direction) {
			order.accept(q -> q.appendOrder("name", direction));
			return this;
		}
	}
	
	/**
	 * Count all Client entries.
	 * @param markedForDeletion Count only valid (false) or only to be deleted (true) or all (null) entries.
	 * @return The number of Client entries.
	 */
	public Long count(@Nullable final Boolean markedForDeletion) {
		return query("SELECT count(*) FROM client")
				.when(markedForDeletion != null, q -> q.append(" WHERE to_be_deleted = ?", markedForDeletion))
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l));
	}
	
	/**
	 * Retrieves a paged subset of optionally filtered Clients.
	 * @param paging Pagination specification.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return Paged subset of matching Client entities.
	 */
	public Paged<ClientPojo> find(final Pagination paging, final BuildingConsumer<ClientService.ClientInquiryBuilder> builder) {
		return builder.prepare(new ClientQueryBuilder()).build(paging).page();
	}
	
	/**
	 * Retrieves all or a filtered subset of Clients.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return All matching Client entities.
	 */
	public List<ClientPojo> find(final BuildingConsumer<ClientService.ClientInquiryBuilder> builder) {
		return builder.prepare(new ClientQueryBuilder()).build(null).all();
	}
	
	/**
	 * Retrieve any Client by its unique ID.
	 * @param clientId ID of the Client.
	 * @return The Client if it exists.
	 */
	public Optional<ClientPojo> find(final UUID clientId) {
		return new ClientQueryBuilder().byUid(clientId).build(null).first();
	}
	
	/**
	 * Retrieve a Client by its ID.
	 * @param clientId ID of the Client.
	 * @param onlyValid Whether to include only Clients not marked for deletion.
	 * @return The Client if it exists.
	 */
	public Optional<ClientPojo> find(final EntityId clientId, final boolean onlyValid) {
		final var q = new ClientQueryBuilder();
		if (clientId.hasUid()) {
			q.byUid(clientId.getUid());
		} else {
			q.byNid(clientId.getNid());
		}
		if (onlyValid) {
			q.withMarkedForDeletion(false);
		}
		return q.build(null).first();
	}
	
	public EntityId put(final ClientPojoPrototype client, final boolean isNew) {
		final EntityId id;
		final QueryBuilder q;
		final FieldBuilder fields = new FieldBuilder();
		
		if (isNew) {
			client.nid.ifDefined(x -> { throw new IllegalArgumentException("Cannot create a Client with predefined numeric ID."); });
			id = EntityId.of(client.uid.orElseNonNull(UUID::randomUUID));
			q = query("INSERT INTO client ");
			fields.add("uid", "?", id.getUid());
		} else {
			id = client.identityProvisional();
			q = query("UPDATE client SET ");
		}
		
		fields.add(client.name.required(isNew), "name", "?", this::validateName);
		fields.and(CustomPropertiesPgDao.addField(client, isNew));
		
		if (isNew) {
			fields.buildInsert(q);
		} else {
			fields.buildUpdate(q);
			q.append(" WHERE ");
			q.appendId(id);
		}
		
		return q.append(" RETURNING uid, nid").first(rs -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))))
			.orElseThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, id.toString()));
	}
	
	private String validateName(String name) {
		name = name.trim();
		if (name.isEmpty()) {
			throw new IllegalArgumentException("Empty string not allowed for Client name.");
		}
		return name;
	}
	
	/**
	 * Deletes a Client entry. Will not delete Client 0.
	 * @param clientId ID of the Client.
	 */
	public void delete(final EntityId clientId) {
		query("DELETE FROM client WHERE nid > 0 AND ").appendId(clientId)
			.updateOrThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, clientId.toString()));
	}
	
	/**
	 * Marks a Client for deletion.
	 * Only works on Clients not already marked for deletion and not on Client 0.
	 * @param clientId ID of the Client.
	 */
	public void markForDeletion(final EntityId clientId) {
		query("UPDATE client SET to_be_deleted = true, name = concat('_TO_BE_DELETED_', uid, '_', name) WHERE ")
			.appendId(clientId).append(" AND nid > 0 AND to_be_deleted = false")
			.updateOrThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, clientId.toString()));
	}
	
	/**
	 * Retrieves a Client's logo by its ID.
	 * @param clientId ID of the client.
	 * @return The logo image BLOB, if found.
	 */
	public Optional<BinaryAttachmentPojo> fetchLogo(final EntityId clientId) {
		return query("SELECT (logo).mime, (logo).data FROM client WHERE ")
				.append(clientId.hasUid() ? "uid" : "nid").append(" = ? AND logo IS NOT NULL", clientId.value())
				.first(rs -> new BinaryAttachmentPojo(rs.getString(1), rs.getBytes(2)));
	}
	
	/**
	 * Stores the logo for a Client.
	 * @param clientId ID of the Client.
	 * @param logo Logo image BLOB.
	 */
	public void updateLogo(final EntityId clientId, @Nullable final BinaryAttachmentPojo logo) {
		query("UPDATE client SET logo = ").with(query -> {
			if (logo != null) {
				query.append("(?, ?)", logo.getMime(), logo.getData());
			} else {
				query.append("null");
			}
		}).append(" WHERE ").appendId(clientId)
		.updateOrThrow(() -> new MiningEntityNotFoundException(ClientPojo.class, clientId.toString()));
	}
	
}
