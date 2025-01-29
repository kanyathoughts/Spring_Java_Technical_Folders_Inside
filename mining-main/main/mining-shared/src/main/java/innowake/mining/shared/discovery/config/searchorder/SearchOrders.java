/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery.config.searchorder;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.InputStream;
import java.io.Serializable;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.io.IOUtils;
import org.w3c.dom.Document;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.filter.AntWildcardFilter;

/**
 * The SearchOrders class is used to resolve a source path to a list of target patterns. 
 * It uses the list of SearchOrder entities passed in the constructor to do the lookup.
 * 
 * For example, if the following list of SearchOrders were given in the constructor:
 * {sourcePattern: "/foo/a/*.cbl", targetPatterns: ["/foo/x/*.cpy", "/foo/y/*.cpy"]}
 * {sourcePattern: "/foo/**\/*.cbl", targetPatterns: ["/foo/z/*.cpy"]  }
 * 
 * Then calling resolvePattern() with "/foo/a/A.cbl" would return ["/foo/x/*.cpy", "/foo/y/*.cpy"] 
 * but calling it with "foo/bar/B.cbl" would return ["/foo/z/*.cpy"].
 * 
 * The path provided to resolvePattern() is matched against the known sourcePatterns using Ant Pattern Matching 
 * See <a href="http://ant.apache.org/manual/dirtasks.html#patterns">http://ant.apache.org/manual/dirtasks.html#patterns</a>
 */
public class SearchOrders implements Serializable {
	
	private static final SearchOrdersAdapter ADAPTER = new SearchOrdersAdapter();
	
	private final List<SearchOrder> searchOrdersList;

	/**
	 * Creates a new instance of SearchOrders with a list of {@link SearchOrder}
	 * 
	 * @param searchOrders List of {@link SearchOrder}
	 */
	public SearchOrders(final List<SearchOrder> searchOrders) {
		searchOrdersList = searchOrders;
	}
	
	/**
	 * Returns the concatenation of all target patterns of all the matching source patterns.
	 * 
	 * @param path The search order pattern. 
	 * @return Target patterns.
	 */
	public String[] resolvePattern(final String path) {
		return searchOrdersList.stream()
			.filter(searchOrder -> searchOrder.getSourcePattern().isPresent())
			.filter(searchOrder -> AntWildcardFilter.match(path, searchOrder.getSourcePattern().get(),
					true, AntWildcardFilter.DEFAULT_SEPARATOR))
			.map(SearchOrder::getTargetPatterns)
			.flatMap(List::stream)
			.toArray(String[]::new);
	}
	
	/**
	 * Returns the list of {@link SearchOrder}
	 *
	 * @return the list of {@link SearchOrder}
	 */
	public List<SearchOrder> getSearchOrdersList() {
		return searchOrdersList;
	}
	
	public Optional<SearchOrder> findMatchingSearchOrder(final String path) {
		return searchOrdersList.stream()
				.filter(searchOrder -> searchOrder.getSourcePattern().isPresent())
				.filter(searchOrder -> AntWildcardFilter.match(path, searchOrder.getSourcePattern().get(),
						true, AntWildcardFilter.DEFAULT_SEPARATOR))
				.findFirst();
	}

	/**
	 * Serializes {@link SearchOrders} returns xml representation of {@link SearchOrder}
	 * 
	 * @return Returns xml representation of {@link SearchOrder} in string format.
	 * @throws Exception Errors when serializing {@link SearchOrders}
	 */
	public String serializeSearchOrders() throws Exception {
		final JAXBContext jaxb = JAXBContext.newInstance(SearchOrdersAdapted.class);
		final Marshaller marshaller = jaxb.createMarshaller();
		final StringWriter writer = new StringWriter();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
		marshaller.marshal(ADAPTER.marshal(this), writer);
		return writer.toString();
	}
	
	/**
	 * Returns a list of {@link SearchOrders} by unmarshalling the given xml.
	 * 
	 * @param xml XML representation of {@link SearchOrder} in string format.
	 * @return List of {@link SearchOrders}.
	 * @throws Exception Errors when deserializing xml.
	 */
	public static List<SearchOrder> loadConfig(final String xml) throws Exception {
		final JAXBContext jaxb = JAXBContext.newInstance(SearchOrdersAdapted.class);
		final Unmarshaller unmarshaller = jaxb.createUnmarshaller();
		final InputStream result = IOUtils.toInputStream(xml, StandardCharsets.UTF_8);
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		final DocumentBuilder db = dbf.newDocumentBuilder();
		final Document document = db.parse(result);
		final SearchOrdersAdapted adapted = (SearchOrdersAdapted) unmarshaller.unmarshal(document);
		final SearchOrders sOrders = ADAPTER.unmarshal(adapted);
		return sOrders.searchOrdersList;
	}
	
	public static class SearchOrdersAdapter extends XmlAdapter<SearchOrdersAdapted, SearchOrders> {

		@Override
		public SearchOrdersAdapted marshal(@Nullable final SearchOrders searchOrders) throws Exception {
			final SearchOrders searchOrdersNN = assertNotNull(searchOrders);
			final SearchOrdersAdapted result = new SearchOrdersAdapted();
			result.searchOrders = searchOrdersNN.searchOrdersList;
			return result;
		}

		@Override
		public SearchOrders unmarshal(@Nullable final SearchOrdersAdapted searchOrdersAdapted) throws Exception {
			final SearchOrdersAdapted searchOrdersAdaptedNN = assertNotNull(searchOrdersAdapted);
			return new SearchOrders(assertNotNull(searchOrdersAdaptedNN.searchOrders));
		}
	}

	@XmlRootElement(name = "search-orders")
	private static class SearchOrdersAdapted {

		@Nullable 
		@XmlElement(name = "search-order")
		private List<SearchOrder> searchOrders;
	}
}
