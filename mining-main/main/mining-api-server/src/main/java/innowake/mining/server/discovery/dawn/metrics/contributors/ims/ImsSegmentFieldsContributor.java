/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.ims;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.ims.ImsParseResultProvider;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.parsing.parser.ims.model.dbdgen.DbdGenModel;
import innowake.ndt.parsing.parser.ims.model.dbdgen.Field;

/**
 * Contributor collecting FieldInfo for IMS Segments. Enabled only
 * when {@code IMS_DBD_FIELDS_ENABLE} is enabled in Discovery config.
 */
@Component
public class ImsSegmentFieldsContributor implements DiscoveryContributor {
	
	private static final Logger LOG = LoggerFactory.getLogger(ImsSegmentFieldsContributor.class);
	private final ParserProviderService parserProvider;
	private final FieldInfoService fieldInfoService;
	private static final String NAME = "NAME";
	private static final String TYPE = "TYPE";
	private static final String BYTES = "BYTES";
	private static final String START = "START";
	private static final String SEQ = "SEQ";

	@Autowired
	public ImsSegmentFieldsContributor(final ParserProviderService parserProvider, final FieldInfoService fieldInfoService) {
		this.parserProvider = parserProvider;
		this.fieldInfoService = fieldInfoService;
	}

	@Override
	public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
		if (context.getConfig().isImsDbdFieldsEnabled()) {
			builder.anchorTo(new ModuleFilter().setTypes(ModuleType.IMS_DBD_SEGMENT), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL,
					ResolutionFlag.RESOLVE_CASE_INSENSITIVE).deferAction("collectDbdFields");
		}
	}

	@SuppressWarnings("unchecked")
	@DeferredAction
	public void collectDbdFields(final DiscoveryContext context, final SourcePojo sourceObject, final ModulePojo module, final ModuleBuilder moduleBuilder) {
		final ImsParseResultProvider imsParser = parserProvider.createImsParser(context);
		final DbdGenModel dbdGenModel;
		try {
			dbdGenModel  = imsParser.getParseResult(sourceObject).createDbdModel();
		} catch (final DiscoveryException e) {
			moduleBuilder.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Failed to parse segment fields: " + e.getMessage(), e);
			LOG.trace("Failed to parse segment fields: ", e);
			return;
		}
		final List<Field> fields = dbdGenModel.getSegments()
				.stream()
				.filter(segment -> segment.getName().equals(module.getName()))
				.flatMap(segment -> segment.getStatements().stream())
				.filter(Field.class::isInstance)
				.map(Field.class::cast)
				.collect(Collectors.toList());
		fields.forEach(field -> {
			final Map<String, Object> properties = new HashMap<>();
			properties.put(TYPE, getProperty(field, TYPE));
			properties.put(BYTES, getProperty(field, BYTES));
			properties.put(START, getProperty(field, START));
			final Object fieldName = field.getProperty(NAME);
			if (fieldName instanceof String) {
				properties.put(NAME, fieldName.toString());
			} else {
				final List<String> fieldNameList = ((List<String>) fieldName);
				if (fieldNameList.contains(SEQ)) {
					properties.put(SEQ, "true");
				}
				properties.put(NAME, fieldNameList.iterator().next());
			}
			fieldInfoService.create(new FieldInfoPojoPrototype()
					.setOrdinal(field.getBytes().intValue())
					.setName(getFieldName(field.getName()))
					.setProperties(properties)
					.setModule(module.identity()));
		});
	}

	private String getProperty(final Field field, final String property) {
		final Object fieldProperty = field.getProperty(property);
		return (fieldProperty != null) ? fieldProperty.toString() : null;
	}
	
	@SuppressWarnings("unchecked")
	private String getFieldName(final Object name) {
		if (name instanceof String) {
			return (String) name;
		} else {
			return ((List<String>) name).iterator().next();
		}
	}
}
