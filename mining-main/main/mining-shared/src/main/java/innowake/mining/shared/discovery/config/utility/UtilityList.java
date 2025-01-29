/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.discovery.config.ConfigResources.UTILITIES;

import java.io.Serializable;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import com.google.common.io.Files;
import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.model.discovery.ResolveTarget;

public class UtilityList implements Serializable {
	public static final String UTILITY_PATH_PROPERTY = "utilityPath";
	public static final String UTILITY_SKIP_MSG = "Skip utility resource file";

	private static final String UTILITY_ENTRY = UTILITIES.getResourceName();

	private final List<UtilityEntity> utilities;
	private final Map<String, UtilityEntity> nameToUtility = new HashMap<>();

	/** Creates a new UtilityList class object from a List element of Utility Entities
	 *
	 * @param utilities a list of UtilityEntity s to form the UtilityList
	 */
	public UtilityList(final List<UtilityEntity> utilities) {
		this.utilities = utilities;
		utilities.forEach(u -> {
			nameToUtility.put(u.getModuleName().toUpperCase(), u);
		});
	}

	public Optional<UtilityEntity> findUtility(final String entryName) {
		return Optional.ofNullable(nameToUtility.get(Files.getNameWithoutExtension(entryName).toUpperCase()));
	}

	/**
	 * Fetches all the utility entities matching the invoking language identified by type.
	 *
	 * @param type Invoking language.
	 * @return List of {@link UtilityEntity} matching invoking language.
	 */
	public @NonNull List<UtilityEntity> getUtilities(@NonNull final ResolveTarget type) {
		return this.utilities.stream().filter(entity -> entity.getInvokingLanguages().contains(type)).collect(Collectors.toList());
	}

	public boolean isUtility(@Nullable final String entryName) {
		if (entryName == null) {
			return false;
		}

		/* Match EXACT utility name. */
		final String entryNameNoExtension = Files.getNameWithoutExtension(entryName).toUpperCase();
		if (nameToUtility.containsKey(entryNameNoExtension)) {
			return true;
		}
		/* No utility prefix present, returning false*/
		return false;
	}

	public List<UtilityEntity> getUtilities() {
		return utilities;
	}

	/**
	 * Check if an entry is a utility and Log skip warnings.
	 *
	 * @param entryName name for utility checking
	 * @param callerLogger Logger to sent the log message
	 * @param className the caller's class name, used for log message
	 *
	 * @return true when the entry is a utility.
	 */
	public boolean filterUtility(final String entryName, final Logger callerLogger, final Supplier<String> className) {
		if (isUtility(entryName)) {
			if (callerLogger.isWarnEnabled()) {
				callerLogger.warn("[{}] {}: {}", className.get(), UTILITY_SKIP_MSG, entryName);
			}
			return true;
		}
		return false;
	}

	/**
	 * serialize the utility list
	 *
	 * @param utilityList the utility list to be serialized
	 * @return serialized utility list in String
	 * @throws Exception error when process utility list
	 */
	public static String serializeUtilityList(final UtilityList utilityList) throws Exception {
		
		final JAXBContext jaxb = JAXBContext.newInstance(UtilityListAdapted.class);
		final Marshaller marshaller = jaxb.createMarshaller();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

		final StringWriter writer = new StringWriter();
		marshaller.marshal(ADAPTER.marshal(utilityList), writer);

		return writer.toString();
	}
	
	/**
	 * Disable utility list by set the list to empty
	 *
	 * @return an empty list
	 */
	public static UtilityList disableUtilityList() {
		return new UtilityList(Collections.emptyList());
	}

	/**
	 * Load the utility list from project or use default list when not found
	 *
	 * @param projectService Access provider for project data.
	 * @param projectId ID of the project
	 * @return utility list
	 * @throws DiscoveryException error when process utility list
	 */
 	public static UtilityList loadUtilityList(final ProjectService projectService, final EntityId projectId) throws DiscoveryException {
		try {
			final JAXBContext jaxb = JAXBContext.newInstance(UtilityListAdapted.class);
			final Unmarshaller unmarshaller = jaxb.createUnmarshaller();
			final String config = projectService.getXmlConfig(projectId, UTILITY_ENTRY);
			final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			final DocumentBuilder db = dbf.newDocumentBuilder();
			final Document document = db.parse(new InputSource((new StringReader(config))));
			final UtilityListAdapted adapted = (UtilityListAdapted) unmarshaller.unmarshal(document);
			return ADAPTER.unmarshal(adapted);
		} catch (final Exception exception) {
			throw new DiscoveryException("Error while loading utilities" , exception);
		}
	}

	/**
	 * Load the utility list from a document
	 *
	 * @param document document containing the utilityList to be loaded
	 * @return utility list
	 * @throws DiscoveryException error when process utility list
	 */
	public static UtilityList loadOutsideUtilityList(final Document document) throws DiscoveryException {
		try {
			final JAXBContext jaxb = JAXBContext.newInstance(UtilityListAdapted.class);
			final Unmarshaller unmarshaller = jaxb.createUnmarshaller();
			final UtilityListAdapted adapted = (UtilityListAdapted) unmarshaller.unmarshal(document);
			return ADAPTER.unmarshal(adapted);
		} catch (final Exception exception) {
			throw new DiscoveryException("Error while loading utilities" , exception);
		}
	}

	private static final UtilityListAdapter ADAPTER = new UtilityListAdapter();

	private static class UtilityListAdapter extends XmlAdapter<UtilityListAdapted, UtilityList> {

		@Override
		public UtilityList unmarshal(@Nullable final UtilityListAdapted adapted) {
			final UtilityListAdapted adaptedNN = assertNotNull(adapted);
			final List<UtilityEntity> utilities = adaptedNN.utilities != null ? adaptedNN.utilities :  Collections.emptyList();
			return new UtilityList(utilities);
		}

		@Override
		public UtilityListAdapted marshal(@Nullable final UtilityList utilityList) throws Exception {
			final UtilityListAdapted result = new UtilityListAdapted();
			result.utilities = assertNotNull(utilityList).utilities;
			return result;
		}

	}

	@XmlRootElement(name="Entities")
	private static class UtilityListAdapted {
		@XmlElement(name="Entity")
		@Nullable
		private List<UtilityEntity> utilities;
	}

}
