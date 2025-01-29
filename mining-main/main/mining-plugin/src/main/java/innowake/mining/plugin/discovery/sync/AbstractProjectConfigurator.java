/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.discovery.sync;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;

import innowake.base.eclipse.common.core.NatureDescription;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.ide.core.IIdeProjectNature;
import innowake.product.base.core.api.ApiException;
import innowake.product.base.core.api.script.ScriptException;

/**
 * Util base class to set natures and builders on a project.
 * Language specific implementations use this class for common operations. *
 */
public abstract class AbstractProjectConfigurator implements ProjectConfigurator {

	protected static final Logger LOG = LoggerFactory.getLogger(AbstractProjectConfigurator.class);

	private final ResolveTarget language;

	public AbstractProjectConfigurator(final ResolveTarget language) {
		this.language = language;
	}

	protected void copy(final String name, final IProject project) throws ApiException {
		copy(name, project, name);
	}

	protected void copy(final String name, final IProject project, final String target) throws ApiException {
		final IFile file = project.getFile(target);

		if (file.exists()) {
			if (LOG.isInfoEnabled()) {
				LOG.info("File {} already exists", target);
			}
			return;
		}

		try (InputStream resource = ProjectConfigurator.class.getResourceAsStream(name)) {
			file.create(resource, true, null);
		} catch (final CoreException e) {
			/* do not use multi catch due to obfuscation problem */
			throw new ScriptException(e.getMessage(), e);
		} catch (final IOException e) {
			/* do not use multi catch due to obfuscation problem */
			throw new ScriptException(e.getMessage(), e);
		}
	}

	/**
	 * Add the nature to the project and start an incremental build.
	 * The build is required that the product api (cobolclipse, natclipse, batchclipse) get noticed and are able to collect the
	 * affected resources.
	 *
	 * @param nature The desired nature to add.
	 * @param project The project to add the nature to.
	 * @throws ApiException Thrown by the eclipse core or build request.
	 */
	protected void addProjectNature(final NatureDescription<? extends IIdeProjectNature> nature, final IProject project) throws ApiException {
		try {
			nature.addNature(project, null);
		} catch (final CoreException exception) {
			throw new ScriptException(exception.getMessage(), exception);
		}
	}

	/* base configuration for removing source files from build classpath */
	@Override
	public void configure(final IProject project) {
		excludeSourceLibs(project, this.language);
	}

	@Override
	public boolean accepts(final Collection<ResolveTarget> identifiedLanguages) {
		return identifiedLanguages.stream().map(ResolveTarget::getLanguage).anyMatch(language::equals);
	}

	/**
	 * Loads properties from a property file.
	 * <p>
	 * <b>Example:</b>
	 * <pre>
	 * loadProperties(project, ".settings/innowake.natclipse.prefs");
	 * </pre>
	 *
	 * @param project the project where the property file is located
	 * @param path the project relative path of the property file
	 * @return the properties of the file or empty properties if file does not exist
	 * @throws IOException if an error occurred when reading from the input stream
	 * @throws IllegalArgumentException if the input stream contains a malformed Unicode escape sequence
	 */
	protected Properties loadProperties(final IProject project, final String path) throws IOException {
		final File file = new File(project.getFile(path).getLocationURI());
		final Properties result = new Properties();

		try (final FileInputStream in = new FileInputStream(file)) {
			result.load(in);
		} catch (final FileNotFoundException e) {
			/* close the stream on exception */
		}

		return result;
	}

	/**
	 * Saves properties into a property file.
	 * <p>
	 * <b>Example:</b>
	 * <pre>
	 * saveProperties(project, ".settings/innowake.natclipse.prefs", props);
	 * </pre>
	 *
	 * @param project the project where the property file is located
	 * @param path the project relative path of an existing or non-existing file
	 * @param properties the properties to save
	 * @throws IOException if an error occurs
	 */
	protected void saveProperties(final IProject project, final String path, final Properties properties) throws IOException {
		final File file = new File(project.getFile(path).getLocationURI());
		FileUtils.forceMkdirParent(file);
		try (final FileOutputStream out = new FileOutputStream(file)) {
			properties.store(out, null);
		}
	}

	/**
	 *
	 * exclude source libs for source folders so they aren't built
	 *
	 * @param project the project that houses the source folders we mean to exclude
	 * @param target the ResolveTarget used to determine what source files we will exclude
	 */
	protected static void excludeSourceLibs(final IProject project, final ResolveTarget target) {

		/* If a java project */
		try {
			if (!project.getDescription().hasNature("org.eclipse.jdt.core.javanature")) {
				return;
			}
		} catch (CoreException e) {
			if( LOG.isErrorEnabled() ) LOG.error("Error getting project description and project nature. Can't add exclude libs to classpath. {}", 
					ExceptionUtils.getFullStackTrace(e));
			return;
		}
		final IJavaProject javaProject = JavaCore.create(project);

		final IPath baseSrcPath = new Path(project.getFullPath() + String.valueOf(IPath.SEPARATOR) + IdentificationMapper.getRelativePathRoot());

		final IPath relSrcPath = new Path(IdentificationMapper.getRelativePathWithoutRoot(target)).addTrailingSeparator();

		final IPath baseMaybePath = new Path(IdentificationMapper.getMaybePathParentWithoutRoot()).addTrailingSeparator();

		final IPath baseDuplicatePath = new Path(IdentificationMapper.getDuplicatePathParentWithoutRoot()).addTrailingSeparator();

		/* Add relative path to exclusion list */
		IClasspathEntry[] rawClasspath;
		try {
			rawClasspath = javaProject.getRawClasspath();
		} catch (JavaModelException e) {
			if( LOG.isErrorEnabled()) LOG.error("Error getting classpath from project. Cannot add exclude libs to this project. {} ", 
					ExceptionUtils.getFullStackTrace(e));
			return;
		}
		
		/* add new source path if for some reason there is none (which would be rare) */
		rawClasspath = addSourcePathIfNotPresent( rawClasspath, baseSrcPath);
		
		/* add exclusions to source path */
		rawClasspath = addSourceExcludePatterns( rawClasspath, baseSrcPath, relSrcPath );

		/* Add maybe path to exclusion list */
		rawClasspath = addSourceExcludePatterns( rawClasspath, baseSrcPath, baseMaybePath );

		/* Add duplicate path to exclusion list */
		rawClasspath = addSourceExcludePatterns( rawClasspath, baseSrcPath, baseDuplicatePath );

		/* update classpath */
		try {
			javaProject.setRawClasspath( rawClasspath, null );
		} catch (JavaModelException e) {
			if(LOG.isErrorEnabled()) LOG.error("Error setting new classpath for project that includes excluded libs. {} ", 
					ExceptionUtils.getFullStackTrace(e));
		}
	}
	
	/**
	 * 
	 * Adds a default source path for this project if none is defined in the classpath.
	 *
	 * @param rawClasspath the classpath entry array to search
	 * @param defSourcePath the def source path to add if none can be found
	 * @return the new classpath entries if one was added or the same entries that were passed to the method
	 */
	protected static IClasspathEntry[]  addSourcePathIfNotPresent( final IClasspathEntry[] rawClasspath, final IPath defSourcePath ) {
		/* loop through all classpath entries, looking for source entry */
		for (final IClasspathEntry c : rawClasspath) {
			if (c.getEntryKind() == IClasspathEntry.CPE_SOURCE && c.getPath() != null && ! c.getPath().isEmpty() ) {
				return rawClasspath;
			}
		}
		

		if(LOG.isInfoEnabled()) LOG.info( "Adding new source entry: ", defSourcePath );
		
		/* if we've looped through everything and have not found a classpath entry for source, then we add one */
		/* add new source entry from scratch */
		final IClasspathEntry newSourceEntry = JavaCore.newSourceEntry(defSourcePath);

		/* return new classpath array after combining with new entry */
		return Stream.concat(Arrays.stream(rawClasspath), Stream.of(newSourceEntry)).collect(Collectors.toList())
				.toArray(new IClasspathEntry[rawClasspath.length + 1]);
	}

	/**
	 *
	 * add exclude patterns to the classpath (if not already present) and return edited classpath
	 *
	 * @param rawClasspath the classpath we will use as a base
	 * @param absSourcePath the source path for the source entry (which should include project name)
	 * @param exclusionPattern the exclusion pattern to add the the source entry (ex: "cobol/")
	 * @return the new classpath that is a result of adding any source entries and exclusions to the classpath passed in
	 */
	protected static IClasspathEntry[] addSourceExcludePatterns(final IClasspathEntry[] rawClasspath, final IPath absSourcePath, final IPath exclusionPattern) {

		/* loop through all classpath entries to find source type that matches abs source path */
		for (final IClasspathEntry c : rawClasspath) {
			if (c.getEntryKind() == IClasspathEntry.CPE_SOURCE && c.getPath().toString().equalsIgnoreCase(absSourcePath.toString())) {

				/* source path already exists, should check if already has exclusion pattern */
				if (Arrays.stream(c.getExclusionPatterns()).anyMatch(e -> e.toString().contains(exclusionPattern.toString()))) {
					/* if we've already found source path and exclusion pattern, we don't add it again */
					if(LOG.isDebugEnabled()) LOG.debug("Exclusion already added to source path: {}...", exclusionPattern);
					return rawClasspath;
				}

				/* Add a new exclusion pattern to existing source path */
				if(LOG.isInfoEnabled()) LOG.info("Adding new exclusion to existing source path: {}... ", exclusionPattern);

				/* add exclusion pattern */
				final IPath[] exclusionPatterns = Arrays.copyOf(c.getExclusionPatterns(), c.getExclusionPatterns().length + 1);
				exclusionPatterns[c.getExclusionPatterns().length] = exclusionPattern;

				/* create new source entry from old that now includes exclusion pattern */
				final IClasspathEntry newSourceEntry = JavaCore.newSourceEntry(c.getPath(), c.getInclusionPatterns(), exclusionPatterns, c.getOutputLocation(),
						c.getExtraAttributes());

				/* return after finding first source entry (and after adding new entry back into classpath entry array */
				return Arrays.stream(rawClasspath).map(ce -> ce.getPath() == newSourceEntry.getPath() ? newSourceEntry : ce).collect(Collectors.toList())
						.toArray(new IClasspathEntry[rawClasspath.length]);
			}
		}
		
		/* Add a new exclusion pattern to existing source path */
		if(LOG.isDebugEnabled()) LOG.debug("No existing source path found to add exclusion pattern {}. No exclusion was added. ", exclusionPattern);
		
		return rawClasspath;
	}
}
