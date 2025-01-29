/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.resolver;

import java.util.List;

import innowake.mining.shared.discovery.ReferenceResolutionException;

/**
 * Base interface for reference resolution.
 * 
 * @param <T> the input type
 */
public interface ReferenceResolver<T> {
	
	/**
	 * Resolve the reference name for given input type.
	 *
	 * @param reference the input type
	 * @return list of resolved names
	 * @throws ReferenceResolutionException 
	 */
    default String resolveToSingle(final T reference) throws ReferenceResolutionException {
        final List<String> resolved = resolve(reference);
        if (resolved.size() != 1) {
            throw new ReferenceResolutionException(ReferenceResolutionException.Cause.MULTIPLE_REFERENCES_FOUND);
        } else if (resolved.isEmpty()) {
            throw new ReferenceResolutionException(ReferenceResolutionException.Cause.NO_REFERENCE_FOUND);
        }
        return resolved.get(0);
    }
    
    /**
	 * Resolve the reference name for given input type.
	 *
	 * @param reference the input type
	 * @return list of resolved names
	 */
    List<String> resolve(T reference);
}
