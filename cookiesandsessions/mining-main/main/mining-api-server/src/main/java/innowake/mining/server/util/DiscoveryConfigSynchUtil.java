/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Utility class for synchronizing reading utilities.xml files and synchronizing UtilityList objects
 */
public class DiscoveryConfigSynchUtil {

	private static final Charset CONFIG_CHARSET = StandardCharsets.UTF_8;
	private static final String resourceName = "/db/migration/discovery-config/" + ConfigResources.UTILITIES.getResourceName();


	/**
	 * Gets a BufferedReader for a given input URL
	 *
	 * @param url the url containing the location of where reader will be
	 * @return BufferedReader of the remote url
	 */
	public static BufferedReader getUtilitiesReaderRemote(final URL url) {
		try {
			final HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			conn.setRequestProperty("Accept", "application/xml");
			return new BufferedReader(new InputStreamReader(conn.getInputStream()));
		}
		catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Gets a Buffered Reader of the local PD/mining Utilities.xml
	 *
	 * @return BufferedReader of the mining utilities.xml
	 */
	public static BufferedReader getUtilitiesReaderLocal() {
		final InputStream stream = DiscoveryConfigSynchUtil.class.getResourceAsStream(resourceName);
		try {
			if (stream == null) {
				final String errorMessage = "File " + resourceName + " not found";
				throw new IOException(errorMessage);
			}
			return new BufferedReader(new InputStreamReader(stream, CONFIG_CHARSET));
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Returns a new UtilityList based on the utility entities that are missing from the utilities in the first reader
	 *
	 * @param readerToUpdate a Buffered reader to the utilities that need to be updated
	 * @param readerToReference Buffered reader to the utilities that will be referenced to update the first readers utilities
	 * @return a Utility List containing the updated utilities contained in the first reader
	 */
	public static UtilityList readAndParseUtilities(final BufferedReader readerToUpdate, final BufferedReader readerToReference) {
		try {
			final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			final DocumentBuilder db = dbf.newDocumentBuilder();
			final Document documentToUpdate = db.parse(new InputSource(readerToUpdate));
			final UtilityList utilityListToUpdate = UtilityList.loadOutsideUtilityList(documentToUpdate);
			final Document documentToReference = db.parse(new InputSource(readerToReference));
			final UtilityList utilityListToReference = UtilityList.loadOutsideUtilityList(documentToReference);
			readerToUpdate.close();
			readerToReference.close();
			return resolveDifferencesInUtilities(utilityListToUpdate, utilityListToReference);
		}
		catch (final Exception e) {
			throw new RuntimeException();
		}
	}

	private static UtilityList resolveDifferencesInUtilities(final UtilityList utilityListToUpdate, final UtilityList utilityListToReference) {
		final List<UtilityEntity> listToUpdate = utilityListToUpdate.getUtilities();
		utilityListToReference.getUtilities().stream()
				.filter(utilityEntity -> ! utilityListToUpdate.isUtility(utilityEntity.getModuleName()))
				.forEach(listToUpdate::add);
		return new UtilityList(listToUpdate);
	}
}
