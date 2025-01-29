/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

/**
 * Flags that control dependency resolution.
 */
public enum ResolutionFlag {
	/**
	 * Flag indicating that target name should be treated case-insensitively when resolving the dependency.
	 */
	RESOLVE_CASE_INSENSITIVE,

	/**
	 * Flag indicating that the target should be created if no matching targets were found. This is only valid for
	 * {@code DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...)} because dependency definitions do not allow defining how the
	 * missing target should be created.
	 */
	CREATE_IF_MISSING,

	/**
	 * Flag indicating that if multiple targets are found, a dependency shall be created to all of them.
	 * <p>
	 * Can not be used in combination with {@link #MULTIPLE_MATCH_RESOLVE_ERROR}.
	 */
	MULTIPLE_MATCH_RESOLVE_ALL,

	/**
	 * Flag indicating that if multiple targets are found, a randomly chosen module is used as target.
	 * <p>
	 * Note that this flag can _not_ be used when declaring dependencies as it will lead to indeterministic behavior.
	 * Doing so will throw {@link IllegalArgumentException}.
	 * However, the flag can be used with {@code DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...)} to check for presence of any module of a certain kind.
	 * <p>
	 * Can not be used in combination with {@link #MULTIPLE_MATCH_RESOLVE_ERROR}.
	 */
	MULTIPLE_MATCH_RESOLVE_ANY,

	/**
	 * Flag indicating that if multiple targets are found, no dependency is created and instead an error message is attached to the source module.
	 * This is the default behavior.
	 * <p>
	 * Can not be used in combination with {@link #MULTIPLE_MATCH_RESOLVE_ANY} or {@link #MULTIPLE_MATCH_RESOLVE_ALL}.
	 */
	MULTIPLE_MATCH_RESOLVE_ERROR,

	/**
	 * Flag indicating that the dependency should be created to the parent of the found module described in the module filter.
	 */
	RESOLVE_TO_PARENT,

	/**
	 * Flag indicating that before resolving this dependency it should be merged together with other dependency definitions on the same module
	 * that have the same target and relationship type. The {@linkplain DependencyDefinitionPojo#getAttributes() attributes} and
	 * {@linkplain DependencyDefinitionPojo#getReachedFromModules() conditional dependencies} of these dependency definitions will be combined.
	 * <p>
	 * Note: this flag has a performance impact. Whenever possible, contributors should <i>avoid</i> declaring duplicate dependencies altogether,
	 * rather than making use of this flag.
	 */
	MERGE_DUPLICATES
}
