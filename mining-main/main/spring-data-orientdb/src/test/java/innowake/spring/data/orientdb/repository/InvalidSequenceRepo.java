/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository;

import innowake.spring.data.orientdb.integration.repository.domain.InvalidSequence;

/**
 * Repository to test an entity with invalid id sequence name.
 */
public interface InvalidSequenceRepo extends OrientRepository<InvalidSequence> {
	
}