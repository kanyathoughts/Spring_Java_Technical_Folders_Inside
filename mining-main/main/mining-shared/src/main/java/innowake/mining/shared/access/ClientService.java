/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.BinaryAttachmentPojo;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Specifies basic functions for accessing the Client database entity.
 */
public interface ClientService {

	/**
	 * Query sort builder for sorting {@code client} entities when performing queries on {@code client} entities.
	 */
	interface ClientOrderBuilder {
		/**
		 * Sorts Clients by their numeric ID.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		ClientOrderBuilder sortNid(SortDirection sort);
		
		/**
		 * Sorts Clients by their name using binary collation.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		ClientOrderBuilder sortName(SortDirection sort);
	}

	/**
	 * Query builder for performing queries on {@code client} entities.
	 */
	interface ClientInquiryBuilder extends ClientOrderBuilder {
		/**
		 * Filters Clients by name.
		 * @param name Pattern matching the names to include.
		 * @return Filter builder.
		 */
		ClientInquiryBuilder withName(String name);
		
		/**
		 * Filters out all Clients up to a certain ID.
		 * @param nid Numeric ID, that which and all below are to be filtered.
		 * @return Filter builder.
		 */
		ClientInquiryBuilder withIdAbove(Long nid);
		
		/**
		 * Filters Clients by the given IDs.
		 * @param nids Set of IDs to include.
		 * @return Filter builder.
		 */
		ClientInquiryBuilder withIds(Collection<Long> nids);
		
		/**
		 * Filters Clients based on their deletion status.
		 * @param toBeDeleted Whether to include only deleted (true) or only valid (false) Clients.  
		 * @return Filter builder.
		 */
		ClientInquiryBuilder withMarkedForDeletion(boolean toBeDeleted);
	}
	
	/**
	 * Retrieves all or a filtered subset of Clients.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return All matching Client entities.
	 */
	List<ClientPojo> find(BuildingConsumer<ClientInquiryBuilder> builder);
	
	/**
	 * Retrieves paged subset of optionally filtered Clients.
	 * @param paging Pagination specification.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return Paged subset of matching Client entities.
	 */
	Paged<ClientPojo> find(Pagination paging, BuildingConsumer<ClientInquiryBuilder> builder);
	
	/**
	 * Retrieve any Client by its ID.
	 * @param uid Unique ID of the Client.
	 * @return The Client if it exists.
	 */
	Optional<ClientPojo> find(UUID uid);
	
	/**
	 * Retrieve a Client by its ID.
	 * @param clientId ID of the Client.
	 * @param onlyValid Whether to include only Clients not marked for deletion.
	 * @return The Client if it exists.
	 */
	Optional<ClientPojo> find(EntityId clientId, boolean onlyValid);
	
	/**
	 * Count all Client entries.
	 * @param markedForDeletion Count only valid (false) or only to be deleted (true) or all entries (null).
	 * @return The number of Client entries.
	 */
	Long count(@Nullable Boolean markedForDeletion);
	
	/**
	 * Retrieve any Client by its ID.
	 * @param uid Unique ID of the Client.
	 * @return The Client if it exists.
	 */
	ClientPojo get(UUID uid);
	
	/**
	 * Retrieve a Client by its ID.
	 * @param clientId Limit eligible Clients to IDs.
	 * @param onlyValid Whether to include only Clients not marked for deletion.
	 * @return The Client if it exists.
	 */
	ClientPojo get(EntityId clientId, boolean onlyValid);
	
	/**
	 * Creates a new Client.
	 * @param name Name for the new Client.
	 * @return Unique ID of the new Client.
	 */
	ClientPojo create(String name);
	
	/**
	 * Creates a new Client.
	 * @param client Specifies the name and optionally a predefined unique ID for the new Client.
	 * @return Unique ID of the new Client (as specified or generated).
	 */
	ClientPojo create(ClientPojoPrototype client);
	
	/**
	 * Renames an existing Client.
	 * @param client Specifies the unique or numeric ID of the Client and a new name for it.
	 */
	void update(ClientPojoPrototype client);
	
	/**
	 * Retrieves a Client's logo.
	 * @param nid Unique ID of the client.
	 * @return The logo image BLOB, if found.
	 */
	BinaryAttachmentPojo getLogo(EntityId nid);
	
	/**
	 * Stores the logo for a Client.
	 * @param clientId ID of the Client.
	 * @param logo Logo image BLOB.
	 */
	void setLogo(EntityId clientId, @Nullable BinaryAttachmentPojo logo);
	
	/**
	 * Marks a Client for deletion.
	 * @param clientId ID of the Client.
	 * @return IDs of the Client.
	 */
	EntityId markForDeletion(EntityId clientId);
	
	/**
	 * Deletes a Client directly.
	 * @param clientId ID of the Client.
	 */
	void deleteDirectly(EntityId clientId);
	
	/**
	 * Deletes all Clients marked for deletion.
	 * @throws Exception In case of an issue.
	 */
	void deleteClients() throws Exception;
	
}
