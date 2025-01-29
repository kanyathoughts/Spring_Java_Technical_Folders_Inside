/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * The state associated with an annotation.
 */
@Entity(name = "WorkingStateEnum")
@MiningDataType(name = "WorkingState")
public enum WorkingState {

	CANDIDATE("CANDIDATE"),
	IN_ANALYSIS("IN_ANALYSIS"),
	FOR_REVIEW("FOR_REVIEW"),
	REJECTED("REJECTED"),
	APPROVED("APPROVED"),
	INVALID("INVALID");

	private static final Map<WorkingState, List<WorkingState>> nextStates = new EnumMap<>(WorkingState.class);

	static {
		nextStates.put(WorkingState.CANDIDATE, Arrays.asList(WorkingState.IN_ANALYSIS));
		nextStates.put(WorkingState.IN_ANALYSIS, Arrays.asList(WorkingState.FOR_REVIEW, WorkingState.REJECTED));
		nextStates.put(WorkingState.FOR_REVIEW, Arrays.asList(WorkingState.APPROVED, WorkingState.INVALID));
	}

	private final String name;

	private WorkingState(final String name) {
		this.name = name;
	}
	
	/**
	 * Returns the name.
	 *
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns the annotation state given a name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name the annotation state is associated with.
	 * @return the annotation state
	 */
	public static WorkingState fromName(final String name) {
		for (final WorkingState value : values()) {
			if (value.getName().equalsIgnoreCase(name)) {
				return value;
			}
		}
		throw new IllegalArgumentException("No enum constant " + name + " available");
	}
	
	/**
	 * Returns the names of all annotation states.
	 *
	 * @return the names of all annotation states
	 */
	public static String[] names() {
		final WorkingState[] values = WorkingState.values();
		return Stream.of(values)
				.map(WorkingState::getName)
				.collect(Collectors.toList())
				.toArray(new String[values.length]);
	}
	
	/**
	 * Returns the next states of the current annotation state.
	 * 
	 * @param annotationState the current annotation state 
	 * @return {@link List} of {@link WorkingState} the
	 *         next states of the current annotation state
	 */
	public static List<WorkingState> nextStates(final WorkingState annotationState) {
		return nextStates.getOrDefault(annotationState, Collections.emptyList());
	}

	/**
	 * Returns the previous state of the current annotation state.
	 * 
	 * @param annotationState the current annotation state
	 * @return {@link Optional} of {@link WorkingState} the previous state of the
	 *         current annotation state
	 */
	public static Optional<WorkingState> previousState(final WorkingState annotationState) {
		switch (annotationState) {
			case FOR_REVIEW:
			case REJECTED:
				return Optional.of(WorkingState.IN_ANALYSIS);
			case IN_ANALYSIS:
				return Optional.of(WorkingState.CANDIDATE);
			case APPROVED:
			case INVALID:
				return Optional.of(WorkingState.FOR_REVIEW);
			case CANDIDATE:
			default:
				return Optional.empty();
		}
	}
	
	/**
	 * Returns the target states of the current annotation state.
	 * 
	 * @param annotationState the current annotation state
	 * @return {@link List} of {@link WorkingState} the list which contains 
	 *         list of previous and next states combined.
	 */
	public static List<WorkingState> getTargetStates(final WorkingState annotationState) {
		final List<WorkingState> nextStates = new ArrayList<>();
		nextStates.addAll(nextStates(annotationState));
		previousState(annotationState).ifPresent(nextStates::add);
		return nextStates;
	}
}
