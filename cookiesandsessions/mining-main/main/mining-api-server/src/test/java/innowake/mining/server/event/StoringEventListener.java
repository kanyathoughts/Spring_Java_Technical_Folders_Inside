/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Event Listener that listens for all events and stores them for test verification.
 */
@Component
public class StoringEventListener {

	private final List<AnnotationEvent> events;
	
	public StoringEventListener() {
		this.events = new CopyOnWriteArrayList<>();
	}

	/**
	 * The list of events seen by the listener.
	 *
	 * @return A list of the events seen by the listener.
	 */
	public List<AnnotationEvent> getEvents()	{
		return events;
	}

	/**
	 * Watches for events and stores them.
	 * @param event The captured event.
	 */
	@EventListener
	public void onEvent(final AnnotationEvent event) {
		events.add(event);
	}

	/**
	 * Clears the collection of stored events.
	 */
	public void clear() {
		events.clear();
	}
}
