/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.discovery.metrics;

import java.util.List;
import java.util.Optional;

import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Common interface to access the module repository.
 */
public interface IModuleRepository extends Iterable<ModelArtifact> {

	/** error message when multiple candidates were found but only one module was expected */
	public static final String ERROR_MULTIPE_CANDIDATES = "Unable to resolve dependency < %s, %s>: Multiple possible candidates found: %s";

	/**
	 * Get all repository entries for a target language or languages.
	 *
	 * @param type The desired language or languages. If a type is specified (example: COBOL_BMS), the language (example: COBOL) will be used instead.
	 * @return The list of repo entries or an empty list.
	 */
	List<ModelArtifact> getLanguageEntries(final ResolveTarget... type);

	/**
	 * Get the repo entry count
	 *
	 * @return The count of elements.
	 */
	int size();

	/**
	 * Gets the physical repository entry for a given <code>IFile</code> instance.
	 *
	 * @param path The file instance.
	 * @return The optional repo entry.
	 */
	Optional<ModelArtifact> getPhysicalEntry(String path);

	/**
	 * Get the repository entry matching a given name and given {@code types}.
	 * 
	 * There are three possible outcomes based on the given name and types.
	 * <li>No matching artifacts - Optional#Empty is returned
	 * <li>One matching artifact - The matching artifact is returned
	 * <li>Multiple matching artifacts - The following operation takes place to best matching artifact: <br>
	 * Based on the order of the given {@code types} and the matching artifacts, if the first matching type has one artifact then that artifact is returned.<br>
	 * Else, the {@linkplain ErrorMarker} with the details of matching artifact information is added to the {@code entry} and returns Optional#Empty.<br>
	 * Example:<br>
	 * If the given name and types are "PROGRAM_A" and [BASIC, COBOL, C] respectively and <br>
	 * <li>if the repo has [[PROGRAM_A, COBOL], [PROGRAM_A, COBOL], [PROGRAM_A, C]] then it returns Optional#Empty and adds the ModelError with the information of the 3 matching artifacts.<br>
	 * <li>if the repo has [[PROGRAM_A, COBOL], [PROGRAM_A, C], [PROGRAM_A, C]] then it returns [PROGRAM_A, COBOL]. Because the first matching artifact based on the order of types is COBOL and it has only one matching artifact.
	 *
	 * @param sourceArtifact the source artifact. If multiple artifacts are found, Model Error will be set on the entry.
	 * @param name the given name.
	 * @param types the given types. The entry is matched in this order.
	 * @return the matching repo entry {@link ModelArtifact}.
	 */
	Optional<ModelArtifact> getEntry(ModelArtifact sourceArtifact, String name, ResolveTarget... types);

	/**
	 * Get the repository entry matching a given {@code contextPath} a given {@code name} and given {@code types}.
	 * 
	 * There are three possible outcomes based on the given name and types.
	 * <li>No matching artifacts - Optional#Empty is returned
	 * <li>One matching artifact - The matching artifact is returned
	 * <li>Multiple matching artifacts - The following operation takes place to best matching artifact: <br>
	 * Based on the order of the given {@code types} and the matching artifacts, if the first matching type has one artifact then that artifact is returned.<br>
	 * Else, the {@linkplain ErrorMarker} with the details of matching artifact information is added to the {@code entry} and returns Optional#Empty.<br>
	 * Example:<br>
	 * If the given name and path and types are "PRG_A" and "src/natural/LIB1/**" and "NATURAL_PROGRAM" respectively and <br>
	 * <li>if the repo has [[src/natural/LIB2/programs/PRG_A.nsp, NATURAL_PROGRAM] and [src/natural/LIB3/programs/PRG_A.nsp, NATURAL_PROGRAM]] then it returns Optional#Empty and adds the ModelError with the information of the 2 matching artifacts.<br>
	 * <li>if the repo has [[src/natural/LIB1/programs/PRG_A.nsp, NATURAL_PROGRAM] and [src/natural/LIB2/programs/PRG_A.nsp, NATURAL_PROGRAM]] then it returns [src/natural/LIB1/programs/PRG_A.nsp, NATURAL_PROGRAM] because it is the only matching artifact based on the path.
	 *
	 * @param sourceArtifact the source artifact. If multiple artifacts are found, Model Error will be set on the entry.
	 * @param searchPathOverride if present, the target module is searched for at the given search path first; otherwise it is located relative to {@code entry},
	 *        according to the configured search orders
	 * @param name the given name.
	 * @param types the given types. The entry is matched in this order.
	 * @return the matching repo entry {@link ModelArtifact}.
	 */
	Optional<ModelArtifact> getEntry(ModelArtifact sourceArtifact, Optional<String> searchPathOverride, String name, ResolveTarget... types);
	
	/**
	 * Get the repository entries matching a given name.
	 *
	 * @param name the given name
	 * @return the matching repo entries
	 */
	List<ModelArtifact> getEntries(String name);

	/**
	 * Get all repository entries matching a given name and given {@code types}.
	 * 
	 * @param name the given name.
	 * @param types the given types. The entry is matched in this order.
	 * @return the matching repo entry {@link ModelArtifact}.
	 */
	List<ModelArtifact> getEntries(String name, ResolveTarget... types);
	
	/**
	 * This method returns all contains references for the given  {@code artifact} and which match with the given {@code types}.
	 * 
	 * @param sourceArtifact the source artifact
	 * @param types the given types. The entry is matched in this order
	 * @return the matching repo entry {@link ModelArtifact}
	 */
	List<ModelArtifact> getEntries(final ModelArtifact sourceArtifact, final ResolveTarget... types);
	
	/**
	 * Get the repository entries matching the given {@code types}.
	 *
	 * @param types the given types. The entry is matched in this order.
	 * @return list of matching {@link ModelArtifact}s
	 */
	List<ModelArtifact> getEntries(ResolveTarget... types);

	/**
	 * Get the repository entries matching a given file name.
	 *
	 * @param filename the given file name.
	 * @return the matching repo entries.
	 */
	List<ModelArtifact> getEntriesByFilename(String filename);

	/**
	 * Get the repository entry matching a given name, types and file.
	 * 
	 * @param sourceArtifact the source artifact. If multiple artifacts are found, Model Error will be set on the entry. 
	 * @param name The given name.
	 * @param path The given file.
	 * @param types The given types.
	 * @return The matching repo entry.
	 */
	Optional<ModelArtifact> getEntry(ModelArtifact sourceArtifact, String name, String path, ResolveTarget... types);

	/**
	 * Returns the repository entry matching a given {@code module} {@link EntityId}.
	 *
	 * @param moduleId the {@code module} {@link EntityId}
	 * @return the matching repo entry.
	 */
	Optional<ModelArtifact> getEntry(EntityId moduleId);
	
	/**
	 * Returns all references of the {@link ModelArtifact} given by {@code moduleRid} for the specified {@code relationship} and {@code direction}.
	 * 
	 * @param moduleId the {@code module} {@link EntityId}
	 * @param relationship the relationship of the {@code reference}
	 * @param direction the direction of the references
	 * @return list of matching {@link ModelArtifact}s
	 */
	List<ModelArtifact> getReferences(EntityId moduleId, RelationshipType relationship, RelationshipDirection direction);

	/**
	 * Gets the matching Module artifacts by path pattern.
	 *
	 * @param sourceArtifact the source modelArtifact
	 * @param namePattern the pattern to match for
	 * @param type the give types
	 * @return list of matching {@link ModelArtifact}s
	 */
	Optional<ModelArtifact> getSimilarNameEntry(ModelArtifact sourceArtifact, String namePattern, ResolveTarget type);
}
