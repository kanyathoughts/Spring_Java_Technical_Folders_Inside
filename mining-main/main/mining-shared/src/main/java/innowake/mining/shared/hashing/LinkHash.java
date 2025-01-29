/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.hashing;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Class to compute the linkHash code for {@linkplain ModulePojo modules}. 
 */
public class LinkHash {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(LinkHash.class);
	
	private LinkHash() { }
	
	/**
	 * Calculates the linkHash for the provided {@linkplain ModulePojo} details
	 *
	 * @param name the name of the {@linkplain ModulePojo}
	 * @param path the path of the {@linkplain ModulePojo}
	 * @param containingPath the containingPath of the {@linkplain ModulePojo}
	 * @param technology the technology of the {@linkplain ModulePojo}
	 * @param type the type of the {@linkplain ModulePojo}
	 * @param containingModuleLinkHash the containing module link hash of the {@linkplain ModulePojo}
	 * @return the calculated linkHash
	 */
	public static String calculateLinkHash(final String name, final String technology, final String type,
			@Nullable final String path, @Nullable final String containingPath, @Nullable final String containingModuleLinkHash) {
		final String pathString;
		final String trimmedPath = StringUtils.trimToNull(path);
		if (trimmedPath == null) {
			if (StringUtils.isBlank(containingPath)) {
				pathString = StringUtils.isBlank(containingModuleLinkHash) ? StringUtils.EMPTY : "ContainedIn:".concat(containingModuleLinkHash);
			} else {
				pathString = "ContainedIn:".concat(containingPath);
			}
		} else {
			pathString = trimmedPath;
		}
		
		final String key = new StringBuilder()
				.append("Module:")
				.append(name).append(",")
				.append(pathString).append(",")
				.append(technology).append(",")
				.append(type).toString();

		try {
			final MessageDigest md = MessageDigest.getInstance("SHA-256");
			final byte[] hashBytes = md.digest(key.getBytes(StandardCharsets.UTF_8));

			/* Truncate the hash to 128 bits (16 bytes) */
			final byte[] truncatedHash = new byte[16];
			System.arraycopy(hashBytes, 0, truncatedHash, 0, 16);

			final String hash = Base62Encoder.encode(truncatedHash);

			LOGGER.debug(() -> String.format("Created module hash for key  [%s] : %s", key, hash));
			return hash;
		} catch (final NoSuchAlgorithmException e) {
			throw new IllegalStateException("No Such Algorithm found ", e);
		}
	}

	/**
	 * Calculates the linkHash for the provided details
	 *
	 * @param name the name of the {@linkplain ModulePojo}
	 * @param technology the technology of the {@linkplain ModulePojo}
	 * @param type the type of the {@linkplain ModulePojo}
	 * @return the calculated linkHash
	 */
	public static String calculateLinkHash(final String name, final Technology technology, final Type type) {
		return calculateLinkHash(name, technology.name(), type.name(), null, null, null);
	}
}
