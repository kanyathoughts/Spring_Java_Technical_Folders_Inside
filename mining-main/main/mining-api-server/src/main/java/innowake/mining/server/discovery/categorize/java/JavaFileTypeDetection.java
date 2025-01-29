/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.java;

import java.util.Set;
import org.apache.commons.io.FilenameUtils;
import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;


/**
 * File type detection for java files.
 */
public class JavaFileTypeDetection extends AbstractFileTypeDetection {

	/**
	 * Constructor to initialize Java File Type Detection.
	 */
	public JavaFileTypeDetection() {
		super(getLanguage());
	}
		
	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		if ("java".equalsIgnoreCase(FilenameUtils.getExtension(resource.getPath()))) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.JAVA_COMPILATION_UNIT, ResolveTarget.JAVA);
		}

		if (FilenameUtils.getName(resource.getPath()).equalsIgnoreCase("pom.xml")) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.JAVA, ResolveTarget.JAVA);
		}
		return null;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.JAVA;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

}
