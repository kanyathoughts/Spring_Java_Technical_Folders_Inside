/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery.config.core;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.io.Files;

import innowake.lib.core.lang.Assert;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Mapping.MappingType;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

public class IdentificationMapper {

	private static final Map<ResolveTarget, String> SRC_MAPPING = new EnumMap<>(ResolveTarget.class);
	private static final Map<ResolveTarget, String> DUPLICATE_MAPPING = new EnumMap<>(ResolveTarget.class);
	private static final Map<ResolveTarget, String> MAYBE_MAPPING = new EnumMap<>(ResolveTarget.class);

	/* Unidentified mappings, can't be static due to the folder name coming from the configuration */
	private final Map<ResolveTarget, String> unidentifiedMapping = new EnumMap<>(ResolveTarget.class);
	private final Map<ResolveTarget, String> duplicatedUnidentifiedMapping = new EnumMap<>(ResolveTarget.class);

	public static final String SRC_ROOT = "src";
	private static final String MAYBE_PARENT = "maybe";
	private static final String DUPLICATE_PARENT = "duplicates";

	private static final String PATH_SEPARATOR = "/";

	static {
		SRC_MAPPING.put(ResolveTarget.NATURAL, SRC_ROOT + "/natural");
		SRC_MAPPING.put(ResolveTarget.JCL, SRC_ROOT + "/jcl");
		SRC_MAPPING.put(ResolveTarget.VMS, SRC_ROOT + "/vms");
		SRC_MAPPING.put(ResolveTarget.BASIC, SRC_ROOT + "/basic");
		SRC_MAPPING.put(ResolveTarget.ORACLE, SRC_ROOT + "/oracle");
		SRC_MAPPING.put(ResolveTarget.IMS, SRC_ROOT + "/ims");
		SRC_MAPPING.put(ResolveTarget.COBOL, SRC_ROOT + "/cobol");
		SRC_MAPPING.put(ResolveTarget.XML, SRC_ROOT + "/xml");
		SRC_MAPPING.put(ResolveTarget.ASSEMBLER, SRC_ROOT + "/asm");
		SRC_MAPPING.put(ResolveTarget.PL1, SRC_ROOT + "/pl1");
		SRC_MAPPING.put(ResolveTarget.BINARY, SRC_ROOT + "/binary");
		SRC_MAPPING.put(ResolveTarget.EASYTRIEVE, SRC_ROOT + "/easytrieve");
		SRC_MAPPING.put(ResolveTarget.C, SRC_ROOT + "/c");
		SRC_MAPPING.put(ResolveTarget.CPP, SRC_ROOT + "/cpp");
		SRC_MAPPING.put(ResolveTarget.CSD, SRC_ROOT + "/csd");
		SRC_MAPPING.put(ResolveTarget.RESOURCE, SRC_ROOT + "/resource");
		SRC_MAPPING.put(ResolveTarget.SQL, SRC_ROOT + "/sql");
		SRC_MAPPING.put(ResolveTarget.JAVA, SRC_ROOT + "/java");
		SRC_MAPPING.put(ResolveTarget.ECL, SRC_ROOT + "/ecl");
		SRC_MAPPING.put(ResolveTarget.CICS, SRC_ROOT + "/cics");
		SRC_MAPPING.put(ResolveTarget.VB, SRC_ROOT + "/vb");
		SRC_MAPPING.put(ResolveTarget.WINDOWS, SRC_ROOT + "/windows");
		SRC_MAPPING.put(ResolveTarget.CSHARP, SRC_ROOT + "/csharp");

		DUPLICATE_MAPPING.put(ResolveTarget.NATURAL, SRC_ROOT + "/" + DUPLICATE_PARENT + "/natural");
		DUPLICATE_MAPPING.put(ResolveTarget.JCL, SRC_ROOT + "/" + DUPLICATE_PARENT + "/jcl");
		DUPLICATE_MAPPING.put(ResolveTarget.VMS, SRC_ROOT + "/" + DUPLICATE_PARENT + "/vms");
		DUPLICATE_MAPPING.put(ResolveTarget.BASIC, SRC_ROOT + "/" + DUPLICATE_PARENT + "/basic");
		DUPLICATE_MAPPING.put(ResolveTarget.ORACLE, SRC_ROOT + "/" + DUPLICATE_PARENT + "/oracle");
		DUPLICATE_MAPPING.put(ResolveTarget.COBOL, SRC_ROOT + "/" + DUPLICATE_PARENT + "/cobol");
		DUPLICATE_MAPPING.put(ResolveTarget.XML, SRC_ROOT + "/" + DUPLICATE_PARENT + "/xml");
		DUPLICATE_MAPPING.put(ResolveTarget.ASSEMBLER, SRC_ROOT + "/" + DUPLICATE_PARENT + "/asm");
		DUPLICATE_MAPPING.put(ResolveTarget.PL1, SRC_ROOT + "/" + DUPLICATE_PARENT + "/pl1");
		DUPLICATE_MAPPING.put(ResolveTarget.BINARY, SRC_ROOT + "/" + DUPLICATE_PARENT + "/binary");
		DUPLICATE_MAPPING.put(ResolveTarget.EASYTRIEVE, SRC_ROOT + "/" + DUPLICATE_PARENT + "/easytrieve");
		DUPLICATE_MAPPING.put(ResolveTarget.C, SRC_ROOT + "/" + DUPLICATE_PARENT + "/c");
		DUPLICATE_MAPPING.put(ResolveTarget.CPP, SRC_ROOT + "/" + DUPLICATE_PARENT + "/cpp");
		DUPLICATE_MAPPING.put(ResolveTarget.CSD, SRC_ROOT + "/" + DUPLICATE_PARENT + "/csd");
		DUPLICATE_MAPPING.put(ResolveTarget.IMS, SRC_ROOT + "/" + DUPLICATE_PARENT + "/ims");
		DUPLICATE_MAPPING.put(ResolveTarget.RESOURCE, SRC_ROOT + "/" + DUPLICATE_PARENT + "/resource");
		DUPLICATE_MAPPING.put(ResolveTarget.SQL, SRC_ROOT + "/" + DUPLICATE_PARENT + "/sql");
		DUPLICATE_MAPPING.put(ResolveTarget.JAVA, SRC_ROOT + "/" + DUPLICATE_PARENT + "/java");
		DUPLICATE_MAPPING.put(ResolveTarget.ECL, SRC_ROOT + "/" + DUPLICATE_PARENT + "/ecl");
		DUPLICATE_MAPPING.put(ResolveTarget.CICS, SRC_ROOT + "/" + DUPLICATE_PARENT + "/cics");
		DUPLICATE_MAPPING.put(ResolveTarget.VB, SRC_ROOT + "/" + DUPLICATE_PARENT + "/vb");
		DUPLICATE_MAPPING.put(ResolveTarget.WINDOWS, SRC_ROOT + "/" + DUPLICATE_PARENT + "/windows");
		DUPLICATE_MAPPING.put(ResolveTarget.CSHARP, SRC_ROOT + "/" + DUPLICATE_PARENT + "/csharp");

		MAYBE_MAPPING.put(ResolveTarget.NATURAL, SRC_ROOT + "/" + MAYBE_PARENT + "/natural");
		MAYBE_MAPPING.put(ResolveTarget.JCL, SRC_ROOT + "/" + MAYBE_PARENT + "/jcl");
		MAYBE_MAPPING.put(ResolveTarget.VMS, SRC_ROOT + "/" + MAYBE_PARENT + "/vms");
		MAYBE_MAPPING.put(ResolveTarget.BASIC, SRC_ROOT + "/" + MAYBE_PARENT + "/basic");
		MAYBE_MAPPING.put(ResolveTarget.ORACLE, SRC_ROOT + "/" + MAYBE_PARENT + "/oracle");
		MAYBE_MAPPING.put(ResolveTarget.COBOL, SRC_ROOT + "/" + MAYBE_PARENT + "/cobol");
		MAYBE_MAPPING.put(ResolveTarget.XML, SRC_ROOT + "/" + MAYBE_PARENT + "/xml");
		MAYBE_MAPPING.put(ResolveTarget.ASSEMBLER, SRC_ROOT + "/" + MAYBE_PARENT + "/asm");
		MAYBE_MAPPING.put(ResolveTarget.PL1, SRC_ROOT + "/" + MAYBE_PARENT + "/pl1");
		MAYBE_MAPPING.put(ResolveTarget.BINARY, SRC_ROOT + "/" + MAYBE_PARENT + "/binary");
		MAYBE_MAPPING.put(ResolveTarget.EASYTRIEVE, SRC_ROOT + "/" + MAYBE_PARENT + "/easytrieve");
		MAYBE_MAPPING.put(ResolveTarget.C, SRC_ROOT + "/" + MAYBE_PARENT + "/c");
		MAYBE_MAPPING.put(ResolveTarget.CPP, SRC_ROOT + "/" + MAYBE_PARENT + "/cpp");
		MAYBE_MAPPING.put(ResolveTarget.CSD, SRC_ROOT + "/" + MAYBE_PARENT + "/csd");
		MAYBE_MAPPING.put(ResolveTarget.IMS, SRC_ROOT + "/" + MAYBE_PARENT + "/ims");
		MAYBE_MAPPING.put(ResolveTarget.RESOURCE, SRC_ROOT + "/" + MAYBE_PARENT + "/resource");
		MAYBE_MAPPING.put(ResolveTarget.SQL, SRC_ROOT + "/" + MAYBE_PARENT + "/sql");
		MAYBE_MAPPING.put(ResolveTarget.JAVA, SRC_ROOT + "/" + MAYBE_PARENT + "/java");
		MAYBE_MAPPING.put(ResolveTarget.ECL, SRC_ROOT + "/" + MAYBE_PARENT + "/ecl");
		MAYBE_MAPPING.put(ResolveTarget.CICS, SRC_ROOT + "/" + MAYBE_PARENT + "/cics");
		MAYBE_MAPPING.put(ResolveTarget.VB, SRC_ROOT + "/" + MAYBE_PARENT + "/vb");
		MAYBE_MAPPING.put(ResolveTarget.WINDOWS, SRC_ROOT + "/" + MAYBE_PARENT + "/windows");
		MAYBE_MAPPING.put(ResolveTarget.CSHARP, SRC_ROOT + "/" + MAYBE_PARENT + "/csharp");
	}

	private final Config config;

	public IdentificationMapper(final Config config) {
		this.config = config;
		unidentifiedMapping.put(ResolveTarget.NONE, config.getUndiscoveredFolder());
		duplicatedUnidentifiedMapping.put(ResolveTarget.NONE, config.getUndiscoveredFolder() + "/" + DUPLICATE_PARENT);
	}

	/**
	 * Uses the current active {@link Config} to resolve the correct String to move an identified
	 * {@link SourcePojo}.
	 *
	 * @param identification the identification to be mapped.
	 * @param sourceObjects map containing all {@linkplain SourcePojo SourceObjects} mapped by their Id
	 * @return the mapped String
	 */
	public Tuple2<String, MappingType> map(final Identification identification, final Map<Long, SourcePojo> sourceObjects) {
		final MappingType mappedAs;
		String relativePath;
		final SourcePojo sourceObject = Assert.assertNotNull(sourceObjects.get(identification.resourceId));
		if (identification.getId() == ID.YES) {
			mappedAs = MappingType.SOLID;
			relativePath = getRelativePath(identification.getTarget());
			if (config.retainDirStructure().booleanValue()) {
				relativePath += getResourceFolderPathString(sourceObject);
			}

		} else if (identification.getId() == ID.MAYBE) {
			mappedAs = MappingType.MAYBE;
			relativePath = getMaybePath(identification.getTarget());
			if (config.retainDirStructure().booleanValue()) {
				relativePath += getResourceFolderPathString(sourceObject);
			}
		} else {
			mappedAs = MappingType.SOLID;
			relativePath = getUnidentifiedPath(identification.getTarget()) + getResourceFolderPathString(sourceObject);
		}

		final String folder = config.getFolder(FilenameUtils.getPath(sourceObject.getPath()), identification.getTarget());

		final String newName = identification.getNewName();
		final String targetPath = resolve(newName != null ? newName : sourceObject.getName(), sourceObject.getPath(), 
											new ResolveInfo(relativePath, folder, getFileExtension(identification)));
		return new Tuple2<>(targetPath, mappedAs);
	}

	/**
	 * Resolves the correct String to move an identified {@link SourcePojo} clashing with a previous identified one.
	 *
	 * @param identification the identification to be mapped
	 * @param sourceObjects map containing all {@linkplain SourcePojo SourceObjects} mapped by their Id
	 * @return the mapped path String
	 */
	public String mapDuplicate(final Identification identification, final Map<Long, SourcePojo> sourceObjects) {
		String relativePath = null;
		if (identification.getTarget() == ResolveTarget.NONE) {
			relativePath = getPath(duplicatedUnidentifiedMapping, identification.getTarget().getLanguage(), "duplicate unidentified");
		} else {
			relativePath = getDuplicatePath(identification.getTarget());
		}
		return mapWithRelativePath(identification, relativePath, sourceObjects);
	}

	private String mapWithRelativePath(final Identification identification, final String relativePath, final Map<Long, SourcePojo> sourceObjects) {
		final SourcePojo sourceObject = Assert.assertNotNull(sourceObjects.get(identification.resourceId));
		final String folder = FilenameUtils.getPath(sourceObject.getPath());
		final String newName = identification.getNewName();
		return resolve(newName != null ? newName : sourceObject.getName(), sourceObject.getPath(), new ResolveInfo(relativePath, folder, getFileExtension(identification)));
	}

	private String getFileExtension(final Identification identification) {
		if ( ! config.retainFileExtension(identification.getTarget().getLanguage())) {
			final Set<String> extensions = ResolveTargetHelper.getExtensions(identification.getTarget());
			if (extensions.size() == 1) { /* if more than one extension is applicable then it should retain the extension */
				return extensions.iterator().next();
			}
		}
		return StringUtils.EMPTY;
	}

	public static String getRelativePathRoot() {
		return SRC_ROOT;
	}

	public static String getRelativePathWithoutRoot(final ResolveTarget target) {
		return getRelativePath(target).replaceFirst(getRelativePathRoot() + "/", "");
	}

	public static String getMaybePathParentWithoutRoot() {
		return MAYBE_PARENT;
	}

	public static String getDuplicatePathParentWithoutRoot() {
		return DUPLICATE_PARENT;
	}

	/**
	 * Get the defined project relative path for a given language.
	 *
	 * @param target The target language. Only top-level language definitions are supported.
	 * @return A String with the relative path for this language. Example: {@code /src/cobol}
	 */
	public static String getRelativePath(final ResolveTarget target) {
		return getPath(SRC_MAPPING, target.getLanguage(), "target");
	}

	public static String getMaybePath(final ResolveTarget target) {
		return getPath(MAYBE_MAPPING, target.getLanguage(), "maybe");
	}

	/**
	 * Retrieves the path to place files that are not identified.
	 * 
	 * @param target The target language. Should only be ResolveTarget.NONE for this method.
	 * @return The path to place files that are not identified.
	 */
	private String getUnidentifiedPath(final ResolveTarget target) {
		return getPath(unidentifiedMapping, target.getLanguage(), "unidentified");
	}

	/**
	 * Retrieves the duplicate path to place files that are not identified.
	 * 
	 * @param target The target language. Should only be ResolveTarget.NONE for this method.
	 * @return The duplicate path to place files that are not identified.
	 */
	private static String getDuplicatePath(final ResolveTarget target) {
		return getPath(DUPLICATE_MAPPING, target.getLanguage(), "duplicate");
	}

	private static String getPath(final Map<ResolveTarget, String> entries, final ResolveTarget desiredLang, final String reason) {
		final String result = entries.get(desiredLang);

		if (result == null) {
			throw new IllegalArgumentException(String.format("Couldn't determine %s folder for language %s.", reason, desiredLang.name()));
		}

		return result;
	}

	private String resolve(final String name, final String path, final ResolveInfo info) {
		final Path result = Paths.get(info.relativePath, info.folder).normalize();

		/* Don't strip off extension if we weren't able to identify an extension */
		if (info.extension.isEmpty()) {
			return result.resolve(FilenameUtils.getName(path)).toString();
		} else {
			return result.resolve(Files.getNameWithoutExtension(name) + "." + info.extension).toString();
		}
	}

	private String getResourceFolderPathString(final SourcePojo resource) {
		/* Remove parent folder and resource name */
		final Path resourcePath = Paths.get(resource.getPath());
		return PATH_SEPARATOR + FilenameUtils.getPath(resourcePath.subpath(1, resourcePath.getNameCount()).toString());
	}

	/**
	 * Checks if the file path is in the {@link ResolveTarget} base path.
	 * 
	 * @param type {@link ResolveTarget} to match.
	 * @param sourceObject The path of {@link SourcePojo} to match.
	 * 
	 * @return {@true} if file's base path matches.
	 */
	public static boolean matchesPath(final ResolveTarget type, final SourcePojo sourceObject) {
		final ResolveTarget language = type.getLanguage();
		final Path maybePath = Paths.get(MAYBE_MAPPING.get(language));
		final Path duplicatePath = Paths.get(DUPLICATE_MAPPING.get(language));
		final Path srcPath = Paths.get(SRC_MAPPING.get(language));

		final Path projectPath = Paths.get(sourceObject.getPath());
		return (projectPath.subpath(1, 2).equals(srcPath.subpath(1, 2)) ||
                projectPath.subpath(1, 3).equals(maybePath.subpath(1, 3)) ||
                projectPath.subpath(1, 3).equals(duplicatePath.subpath(1, 3)));
	}

	private static class ResolveInfo {

		/**
		 * The path relative to the project base where all files of one type shall
		 * reside.
		 */
		protected final String relativePath;

		/**
		 * The folder as identified by {@link Config}
		 */
		protected final String folder;

		/**
		 * The extension of the detected file.
		 */
		protected final String extension;

		protected ResolveInfo(final String relativePath, final String folder, final String extension) {
			this.relativePath = relativePath;
			this.folder = folder;
			this.extension = extension;
		}

	}

}
