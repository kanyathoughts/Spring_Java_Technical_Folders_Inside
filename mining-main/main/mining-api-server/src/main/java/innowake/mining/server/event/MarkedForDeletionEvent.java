/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.event;

import innowake.mining.data.event.ClusteredEvent;

/**
 * Event to be fired when an entity (like Client, Project) is marked for asynchronous deletion.
 */
public class MarkedForDeletionEvent extends ClusteredEvent {

	/* Event indicates that something has been marked for deletion in the DB. No additional properties are required */
}
