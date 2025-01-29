/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import javax.persistence.PersistenceException;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.mining.shared.access.TokenInfoService;
import innowake.mining.shared.access.TokenInfoService.OfflineTokenInquiryBuilder;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.security.OfflineTokenInfo;

/**
 * Postgres specific access methods for stored authentication tokens.
 */
public class TokenPgDao extends PgDao {
	
	public class OfflineTokenQueryBuilder implements TokenInfoService.OfflineTokenInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		private boolean subjectFiltered = false;
		
		public List<OfflineTokenInfo> build(final String forSubject) {
			return query("SELECT id, subject, username, description, created, bearer_token FROM offline_token")
				.with(filters::build)
				.append(" ORDER BY created DESC")
				.toList((rs, n) -> {
					final String subject = rs.getString(2);
					return new OfflineTokenInfo((UUID) rs.getObject(1), subject,
							rs.getString(3), rs.getString(4),
							forSubject.equals(subject) ? rs.getString(6) : "*****", "*****")
						.setDateCreated(rs.getTimestamp(5).toInstant());
					});
		}
		
		public int buildDelete() {
			if (! subjectFiltered) {
				throw new IllegalArgumentException("Must specify subject for deletion");
			}
			return query("DELETE FROM offline_token where id = ? and subject = ?").with(filters::build).update();
		}
		
		@Override
		public OfflineTokenInquiryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("id = ?", id));
			return this;
		}
		
		@Override
		public OfflineTokenInquiryBuilder ofSubject(final String subject) {
			filters.accept(q -> q.append("subject = ?", subject));
			subjectFiltered = true;
			return this;
		}
	}
	
	public TokenPgDao(final JdbcTemplate jdbc) {
		super(jdbc);
	}
	
	public List<OfflineTokenInfo> findOfflineTokens(final String subject, final BuildingConsumer<TokenInfoService.OfflineTokenInquiryBuilder> builder) {
		return builder.prepare(new OfflineTokenQueryBuilder()).build(subject);
	}
	
	public Optional<String> findRefreshToken(final String bearerToken) {
		return query("SELECT refresh_token FROM offline_token WHERE bearer_token = ?").addArg(bearerToken)
				.first(rs -> rs.getString(1));
	}
	
	public void storeToken(final OfflineTokenInfo token) {
		query("INSERT INTO offline_token (id, subject, username, description, bearer_token, refresh_token) VALUES (?, ?, ? ,?, ?, ?)")
			.addArgs(token.getId(), token.getSubject(), token.getUsername(),
					token.getDescription(), token.getBearerToken(), token.getRefreshToken())
			.updateOrThrow(() -> new PersistenceException("Failed to store offline token"));
	}
	
	public int deleteToken(final BuildingConsumer<TokenInfoService.OfflineTokenInquiryBuilder> builder) {
		return builder.prepare(new OfflineTokenQueryBuilder()).buildDelete();
	}
	
	public boolean updateTokenDescription(final UUID id, final String subject, final String description) {
		return query("UPDATE offline_token SET description = ? WHERE id = ? AND subject = ?").addArgs(description, id, subject).update() > 0;
	}
	
}
