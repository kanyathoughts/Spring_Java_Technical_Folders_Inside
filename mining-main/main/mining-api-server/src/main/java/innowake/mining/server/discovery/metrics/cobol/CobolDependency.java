/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.AbstractSourceObjectDependency;
import innowake.mining.server.discovery.metrics.SourceObjectDependency;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Cobol implementation of {@link SourceObjectDependency}.
 */
public class CobolDependency extends AbstractSourceObjectDependency {

	final boolean fromDictionary;
	
	@Nullable
	private String libraryName;
	
	@Nullable
	private ModuleLocation moduleLocation;

	CobolDependency(
			final SourcePojo source, 
			final String targetName, 
			final boolean fromDictionary, 
			final SourceObjectResolver sourceObjectResolver) {
		
		super(Technology.COBOL, source, targetName, sourceObjectResolver);
		this.fromDictionary = fromDictionary;
	}
	
	CobolDependency(
			final SourcePojo source, 
			final String targetName, 
			final Type targetType, 
			final boolean fromDictionary, 
			final SourceObjectResolver sourceObjectResolver) {
		
		super(Technology.COBOL, source, targetName, targetType, sourceObjectResolver);
		this.fromDictionary = fromDictionary;
	}

	CobolDependency(
			final SourcePojo source, 
			final String targetName, 
			final Type targetType, 
			final boolean fromDictionary, 
			final SourceObjectResolver sourceObjectResolver,
			final ModuleLocation moduleLocation ) {
		
		super(Technology.COBOL, source, targetName, targetType, sourceObjectResolver);
		this.fromDictionary = fromDictionary;
		this.moduleLocation = moduleLocation;
	}
	
	/**
	 * Returns whether this reference is a copy reference originating from a {@code COPY FROM DICTIONARY} statement.
	 * <p>
	 * This statement is available within HP Cobol and uses a special pre-compiler generating data structures for the Oracle CDO system.
	 *
	 * @return {@code true} if this reference originates from a {@code COPY FROM DICTIONARY} statement, {@code false} otherwise
	 */
	public boolean isFromDictionary() {
		return fromDictionary;
	}

	
	/**
	 * return the libraryName in the given CobolCopyNode.
	 *
	 * @return libraryName present in CobolCopyNode
	 */
	@Nullable
	public String getLibraryName() {
		return libraryName;
	}

	
	/**
	 * Set the libraryName in the given CobolCopyNode.
	 *
	 * @param libraryName name of the library to be set
	 */
	public void setLibraryName(final String libraryName) {
		this.libraryName = libraryName;
	}
	

	/**
	 * return @link ModuleLocation} of CobolCopyNode.
	 *
	 * @return @link ModuleLocation} of CobolCopyNode
	 */
	@Nullable
	public ModuleLocation getModuleLocation() {
		return moduleLocation;
	}

	/**
	 * Set the {@link ModuleLocation} of CobolCopyNode.
	 *
	 * @param moduleLocation {@link ModuleLocation}
	 */
	public void setModuleLocation(final ModuleLocation moduleLocation) {
		this.moduleLocation = moduleLocation;
	}
}
