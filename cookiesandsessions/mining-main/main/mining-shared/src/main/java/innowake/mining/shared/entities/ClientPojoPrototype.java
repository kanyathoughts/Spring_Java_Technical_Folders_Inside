/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.entities;

import innowake.mining.shared.Definable;

/**
 * Client entity request class.
 */
public final class ClientPojoPrototype extends MiningSequentialPojoPrototype<ClientPojoPrototype> {
	
	public final Definable<String> name = new Definable<>(false, "Client.name");
	
	public ClientPojoPrototype() {
		super("Client");
	}
	
	public ClientPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
	
}
