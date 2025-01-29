/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.util.filter.AntWildcardFilter;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.Candidate;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Utility for applying {@link ModuleFilter} to {@link ModelArtifact}.
 */
@Service
public class ModuleFilterUtil {
	
	@Autowired
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;

	
	
	/**
	 * Returns whether the given {@code moduleFilter} matches the given {@code artifact}.
	 * Note that the method returns {@code true} if the filter is empty, i.e. if it contains no criteria.
	 * @param moduleFilter the filter object
	 * @param artifact the model artifact
	 * @return {@code true} if the artifact matches the criteria of the filter, otherwise {@code false}
	 */
	public boolean matches(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		return matchId(moduleFilter, artifact)
				&& matchNames(moduleFilter, artifact)
				&& matchTypes(moduleFilter, artifact)
				&& matchPaths(moduleFilter, artifact)
				&& matchPathPatterns(moduleFilter, artifact)
				&& matchContainedIn(moduleFilter, artifact);
	}

	/**
	 * Returns whether the given {@code moduleFilter} matches the given {@code module}.
	 * Note that the method returns {@code true} if the filter is empty, i.e. if it contains no criteria.
	 * @param context The discovery context
	 * @param moduleFilter the filter object
	 * @param module the {@link ModuleLightweightPojo} object
	 * @return {@code true} if the artifact matches the criteria of the filter, otherwise {@code false}
	 */
	public boolean matches(final DiscoveryContext context, final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		return matchId(moduleFilter, module)
				&& matchNames(moduleFilter, module)
				&& matchTypes(moduleFilter, module)
				&& matchPaths(moduleFilter, module)
				&& matchPathPatterns(moduleFilter, module)
				&& matchContainedIn(context, moduleFilter, module);
	}
	
	/**
	 * Returns whether the given {@code moduleFilter} matches the given {@code ModuleFilter}.
	 * Note that the method returns {@code true} if the properties of one or both the filters is empty.
	 * @param originalFilter the module filter.
	 * @param targetFilter the search order module filter.
	 * @return {@code true} if one filter matches the criteria of the other filter, otherwise {@code false}
	 */
	public boolean matches(final ModuleFilter originalFilter, final ModuleFilter targetFilter) {
		return matchId(originalFilter, targetFilter)
		&& matchNames(originalFilter, targetFilter)
		&& matchTypes(originalFilter, targetFilter)
		&& matchPaths(originalFilter, targetFilter)
		&& matchPathPatterns(originalFilter, targetFilter)
		&& matchContainedIn(originalFilter, targetFilter);		
	}

	/**
	 * Returns combined filter from the given two filters.
	 * Note that the method returns {@link Optional#empty()} if the filters doesn't match and 
	 * returns the originalFilter if it contains any moduleIds.
	 * @param originalFilter the {@link ModuleFilter} the moduleFilter
	 * @param targetFilter the {@link ModuleFilter} the search order module filter.
	 * @return Optional combined{@linkplain ModuleFilter} if the filters match otherwise {@linkplain Optional#empty()}
	 */
	public Optional<ModuleFilter> joinModuleFilter(final ModuleFilter originalFilter, final ModuleFilter targetFilter) {
		/* Return the orginalFilter if it contains any moduleIds */
		if ( ! originalFilter.getModuleIds().isEmpty()) {
			return Optional.of(originalFilter);
		}
		
		/* We will only combine the two filters, if they match*/
		if ( ! matches(originalFilter, targetFilter)) {
			return Optional.empty();
		}
		final ModuleFilter combinedFilter = new ModuleFilter();
		final Optional<ModuleFilter> originalContainedIn = originalFilter.getContainedIn();
		final Optional<ModuleFilter> targetFilterContainedIn = targetFilter.getContainedIn();
		final Optional<ModuleFilter> originalNotFilter = originalFilter.getNot();
		final Optional<ModuleFilter> targetNotFilter = targetFilter.getNot();
		
		final Collection<String> combinedNames = joinFilterProperties(originalFilter.getNames(), targetFilter.getNames());
		final Collection<ModuleType> combinedTypes = joinFilterProperties(originalFilter.getTypes(), targetFilter.getTypes());
		final Collection<String> combinedPaths = joinFilterProperties(originalFilter.getPaths(), targetFilter.getPaths());
		final Set<String> combinedPathPatterns = joinPathPatterns(originalFilter.getPathPatterns(), targetFilter.getPathPatterns());
		final Optional<ModuleFilter> combinedContainedIn = joinNestedFilter(originalContainedIn, targetFilterContainedIn);
		final Optional<ModuleFilter> combinedNotFilter = joinNestedFilter(originalNotFilter, targetNotFilter);
		
		combinedFilter.setNames(combinedNames);
		combinedFilter.setTypes(combinedTypes);
		combinedFilter.setPaths(combinedPaths);
		combinedFilter.setPathPatterns(combinedPathPatterns);
		combinedContainedIn.ifPresent(combinedFilter::setContainedIn);
		combinedNotFilter.ifPresent(combinedFilter::setNot);
		
		return Optional.of(combinedFilter);
	}
	
	private <T> Collection<T> joinFilterProperties(final Set<T> originalProperty, final Set<T> targetProperty) {
		if (originalProperty.isEmpty() && targetProperty.isEmpty()) {
			return Collections.emptySet();
		} else if (! originalProperty.isEmpty() && ! targetProperty.isEmpty()) {
			return CollectionUtils.intersection(originalProperty, targetProperty);
		} else if (originalProperty.isEmpty()) {
			return targetProperty;
		} else {
			return originalProperty;
		}
	}
	
	private Optional<ModuleFilter> joinNestedFilter(final Optional<ModuleFilter> originalFilter, final Optional<ModuleFilter> targetFilter) {
		
		if ( ! originalFilter.isPresent() &&  ! targetFilter.isPresent()) {
			return Optional.empty();
		}
		else if (originalFilter.isPresent() && targetFilter.isPresent()) {
			return joinModuleFilter(originalFilter.get(), targetFilter.get());
		}
		else if (originalFilter.isPresent()) {
			return originalFilter;
		} else {
			return targetFilter;
		}
	}
	
	private Set<String> joinPathPatterns(final Set<String> orignalPathPatterns, final Set<String> targetPathPatterns) {
		
		if (orignalPathPatterns.isEmpty() && targetPathPatterns.isEmpty()) {
			return Collections.emptySet();
		} else if( ! orignalPathPatterns.isEmpty() && ! targetPathPatterns.isEmpty()) {
			return getMatchingPathPatterns(orignalPathPatterns, targetPathPatterns);
		} else if( ! orignalPathPatterns.isEmpty()) {
			return orignalPathPatterns;
		} else {
			return targetPathPatterns;
		}
	}

	private Set<String> getMatchingPathPatterns(final Set<String> orignalPathPatterns, final Set<String> targetPathPatterns) {
		final Set<String> pathPatterns = new HashSet<>();
		for (final String pattern : orignalPathPatterns) {
			 final boolean match = targetPathPatterns.stream().anyMatch(pattern2 -> {
				 /* we need to replace wild card character * with any random character for AntWildCardFilter matcher to work */
				 final String modifiedPattern = pattern2.replace("*", "X");
				 return AntWildcardFilter.match(modifiedPattern, pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			 });
			 if (match) {
				 pathPatterns.add(pattern);
			 }
		}
		
		for (final String pattern : targetPathPatterns) {
			 final boolean match =  orignalPathPatterns.stream().anyMatch(pattern2 -> {
				 final String modifiedPattern = pattern2.replace("*", "X");
				 return AntWildcardFilter.match(modifiedPattern, pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			 });
			 if (match) {
				pathPatterns.add(pattern);
			 }
		}
		return pathPatterns;
	}
	
	private boolean matchId(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		final Set<EntityId> moduleIds = moduleFilter.getModuleIds();
		if ( ! moduleIds.isEmpty() && artifact.getModuleId() != null) {
			return EntityId.contains(moduleIds, Objects.requireNonNull(artifact.getModuleId()));
		}
		return true;
	}

	private boolean matchId(final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		final Set<EntityId> moduleIds = moduleFilter.getModuleIds();
		if ( ! moduleIds.isEmpty()) {
			return EntityId.contains(moduleIds, module.identity());
		}
		return true;
	}

	private boolean matchId(final ModuleFilter moduleFilter, final ModuleFilter otherFilter) {
		final Set<EntityId> moduleIds = moduleFilter.getModuleIds();
		if (moduleIds.isEmpty() || otherFilter.getModuleIds().isEmpty()) {
			return true;
		}
		return CollectionUtils.containsAny(moduleIds, otherFilter.getModuleIds());
	}

	private boolean matchNames(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		if (moduleFilter.getNames().isEmpty()) {
			return true;
		}
		return moduleFilter.getNames().stream().anyMatch(artifact.getName()::equals);
	}

	private boolean matchNames(final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		if (moduleFilter.getNames().isEmpty()) {
			return true;
		}
		return moduleFilter.getNames().stream().anyMatch(module.getName()::equals);
	}

	private boolean matchNames(final ModuleFilter moduleFilter, final ModuleFilter otherFilter) {
		if (moduleFilter.getNames().isEmpty() || otherFilter.getNames().isEmpty()) {
			return true;
		}
		return CollectionUtils.containsAny(moduleFilter.getNames(), otherFilter.getNames());
	}

	private boolean matchPathPatterns(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		if (moduleFilter.getPathPatterns().isEmpty()) {
			return true;
		}
		/* for path we always also check the artifact if the artifact itself does not have a path */
		return moduleFilter.getPathPatterns().stream().anyMatch(pattern -> {
			if (artifact.getPath().isPresent()) {
				return AntWildcardFilter.match(artifact.getPath().get(), pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			} else if (artifact.getParentPath().isPresent()) {
				return AntWildcardFilter.match(artifact.getParentPath().get(), pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			} else {
				return "**/*".equals(pattern);
			}
		});
	}

	private boolean matchPathPatterns(final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		if (moduleFilter.getPathPatterns().isEmpty()) {
			return true;
		}
		/* for path we always also check the artifact if the artifact itself does not have a path */
		return moduleFilter.getPathPatterns().stream().anyMatch(pattern -> {
			if (module.getPath() != null ) {
				return AntWildcardFilter.match(module.getPath(), pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			} else if (module.getParentPath() != null) {
				return AntWildcardFilter.match(module.getParentPath(), pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			} else {
				return "**/*".equals(pattern);
			}
		});
	}

	private boolean matchPathPatterns(final ModuleFilter moduleFilter, final ModuleFilter otherFilter) {
		if (moduleFilter.getPathPatterns().isEmpty() || otherFilter.getPathPatterns().isEmpty()) {
			return true;
		}
		for (final String pattern : moduleFilter.getPathPatterns()) {
			 final boolean match = otherFilter.getPathPatterns().stream().anyMatch(pattern2 -> {
				 final String modifiedPattern = pattern2.replace("*", "X");
				 return AntWildcardFilter.match(modifiedPattern, pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			 });
			 if (match) {
				 return match;
			 }
		}
		
		for (final String pattern : otherFilter.getPathPatterns()) {
			 final boolean match = moduleFilter.getPathPatterns().stream().anyMatch(pattern2 -> {
				 final String modifiedPattern = pattern2.replace("*", "X");
				 return AntWildcardFilter.match(modifiedPattern, pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR);
			 });
			 if (match) {
				 return match;
			 }
		}
		return false;
	}

	private boolean matchPaths(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		if (moduleFilter.getPaths().isEmpty()) {
			return true;
		}
		/* for path we always also check the artifact if the artifact itself does not have a path */
		return moduleFilter.getPaths().stream().anyMatch(path -> {
			if (artifact.getPath().isPresent()) {
				return artifact.getPath().get().equals(path);
			} else if (artifact.getParentPath().isPresent()) {
				return artifact.getParentPath().get().equals(path);
			} else {
				return false;
			}
		});
	}

	private boolean matchPaths(final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		if (moduleFilter.getPaths().isEmpty()) {
			return true;
		}
		/* for path we always also check the artifact if the artifact itself does not have a path */
		return moduleFilter.getPaths().stream().anyMatch(path -> {
			if (module.getPath() != null ) {
				return assertNotNull(module.getPath()).equals(path);
			} else if (module.getParentPath() != null) {
				return assertNotNull(module.getParentPath()).equals(path);
			} else {
				return false;
			}
		});
	}

	private boolean matchPaths(final ModuleFilter moduleFilter, final ModuleFilter otherFilter) {
		if (moduleFilter.getPaths().isEmpty() || otherFilter.getPaths().isEmpty() ) {
			return true;
		}
		return CollectionUtils.containsAny(moduleFilter.getPaths(), otherFilter.getPaths());
	}

	private boolean matchTypes(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		if (moduleFilter.getTypes().isEmpty()) {
			return true;
		}
		return moduleFilter.getTypes().stream().anyMatch(languageType -> {
			final ResolveTarget resolveTarget = ResolveTargetHelper.fromTechnologyAndType(languageType.getTechnology(), languageType.getType());
			return artifact.getType().equals(resolveTarget);
		});
	}

	private boolean matchTypes(final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		if (moduleFilter.getTypes().isEmpty()) {
			return true;
		}
		return moduleFilter.getTypes().stream().anyMatch(languageType -> {
			final ResolveTarget resolveTarget = ResolveTargetHelper.fromTechnologyAndType(languageType.getTechnology(), languageType.getType());
			return ResolveTargetHelper.fromTechnologyAndType(module.getModuleType().getTechnology(), module.getModuleType().getType()).equals(resolveTarget);
		});
	}

	private boolean matchTypes(final ModuleFilter moduleFilter, final ModuleFilter otherFilter) {
		if (moduleFilter.getTypes().isEmpty() || otherFilter.getTypes().isEmpty()) {
			return true;
		}
		return CollectionUtils.containsAny(moduleFilter.getTypes(), otherFilter.getTypes());
	}
	
	private boolean matchContainedIn(final ModuleFilter moduleFilter, final ModelArtifact artifact) {
		final Optional<ModuleFilter> containedIn = moduleFilter.getContainedIn();
		if (containedIn.isPresent()) {
			if (artifact.getParentModule() == null) {
				/* matcher for parent present but not parent exists */
				return false;
			}
			return matches(containedIn.get(), assertNotNull(artifact.getParentModule()));
		}
		return true;
	}
	
	private boolean matchContainedIn(final DiscoveryContext context, final ModuleFilter moduleFilter, final ModuleLightweightPojo module) {
		final Optional<ModuleFilter> containedIn = moduleFilter.getContainedIn();
		if (containedIn.isPresent()) {
			final var parent = module.getParent();
			if (parent == null) {
				return false;
			}
			final ModuleLightweightPojo parentModule =
					orientDiscoveryPersistence.fetchModulesLightWeight(context, Collections.singletonList(parent)).get(0);
			return matches(context, containedIn.get(), assertNotNull(parentModule));
		}
		return true;
	}
 
	private boolean matchContainedIn(final ModuleFilter moduleFilter, final ModuleFilter otherFilter) {
		final Optional<ModuleFilter> containedIn = moduleFilter.getContainedIn();
		final Optional<ModuleFilter> otherContainedIn = otherFilter.getContainedIn();
		if (containedIn.isPresent() && otherContainedIn.isPresent()) {
			return matches(containedIn.get(), otherContainedIn.get());
		}
		return true;
	}
	
	/**
	 * Returns {@link ModuleFilter} by converting the given {@link Candidate} into {@link ModuleFilter}.
	 * 
	 * @param searchOrder the {@link Candidate}
	 * @param contextPath the context path
	 * @return {@link ModuleFilter}
	 */
	public ModuleFilter toModuleFilter(final Candidate searchOrder, final Optional<String> contextPath) {
		final ModuleFilter moduleFilter = new ModuleFilter();
		final Optional<String> name = searchOrder.getName();
		if (name.isPresent()) {
			moduleFilter.setNames(name.get());
		}
		final Optional<String> path = searchOrder.getPath();
		if (path.isPresent()) {
			moduleFilter.setPaths(path.get());
		}
		final Optional<String> type = searchOrder.getType();
		if (type.isPresent()) {
			moduleFilter.setTypes(ModuleType.valueOf(type.get()));
		}
		final Optional<String> optionalPathPattern = searchOrder.getPathPattern();
		if (optionalPathPattern.isPresent()) {
			final String pathPattern = optionalPathPattern.get();
			if (pathPattern.startsWith("./")) {
				final String pathWithoutStartingDot = pathPattern.substring(2);
				final String finalPath = contextPath.isPresent() ? String.format("%s%s", FilenameUtils.getPath(contextPath.get()), pathWithoutStartingDot)
						: pathWithoutStartingDot;
				moduleFilter.setPathPatterns(finalPath);
			} else {
				moduleFilter.setPathPatterns(pathPattern);
			}
		}
		final Optional<Candidate> containedIn = searchOrder.getContainedIn();
		if (containedIn.isPresent()) {
			moduleFilter.setContainedIn(toModuleFilter(containedIn.get(), contextPath));
		}
		return moduleFilter;
	}
}
