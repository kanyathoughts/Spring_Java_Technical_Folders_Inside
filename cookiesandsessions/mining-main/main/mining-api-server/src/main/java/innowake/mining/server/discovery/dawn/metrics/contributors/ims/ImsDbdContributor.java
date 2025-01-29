/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.ims;

import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_ACCESS_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_DBD_DATASET;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_DBD_SEGMENT_COMPRTN;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_DBD_SEGMENT_PARENT;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_OS_ACCESS_TYPE;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeComparator;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.parsing.parser.ims.model.dbdgen.DbdGenModel;
import innowake.ndt.parsing.parser.ims.model.dbdgen.Lchild;
import innowake.ndt.parsing.parser.ims.model.dbdgen.Segment;

/**
 * Contributor for IMS_DBD and its related subtypes.
 */
@Component
public class ImsDbdContributor implements DiscoveryContributorFromSource {
	
	private static final Logger LOG = LoggerFactory.getLogger(ImsDbdContributor.class);
	
	@Autowired
	private ParserProviderService parserProviderService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		final var type = sourceObject.getType();
		return sourceObject.getTechnology() == Technology.IMS
				&& (type == Type.DBD || type == Type.DBD_COMPRTN || type == Type.DBD_DATASET || type == Type.HDAMPARM || type == Type.HELP);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final var imsParseResultProvider = parserProviderService.createImsParser(context);
		final var moduleType = ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType());
		try {
			final ModuleBuilder rootModule;
			String name = sourceObject.getName();
			if (moduleType == ModuleType.IMS_DBD || moduleType == ModuleType.IMS_HDAMPARM) {
				final var imsParseResult = imsParseResultProvider.getParseResult(sourceObject);
				final var dbdGenModel = imsParseResult.createDbdModel();

				if (moduleType == ModuleType.IMS_DBD) {
					/* The NAME property defines the actual system resource name for the environment if present, instead of the physical file name */
					name = getDbdNameIfNotBlank(dbdGenModel, name);
				}
				rootModule = builder.declareRootModule(name, moduleType);
				final Set<String> statements = new HashSet<>();
				collectDbdAttributes(rootModule, dbdGenModel, statements);
				if (moduleType == ModuleType.IMS_DBD) {
					collectSubModulesAndDependencies(builder, dbdGenModel, rootModule, sourceObject.getPath());
				}
			} else {
				rootModule = builder.declareRootModule(name, moduleType);
			}
			final var metricsV2 = ImsMetricsUtil.calculateSourceMetrics(sourceObject, rootModule);
			rootModule.addAdditionalInfo(metricsV2);
		} catch (final DiscoveryException e) {
			LOG.error("[" + sourceObject.getName() + "] Error during parsing of DBD: " + sourceObject.getName(), e);
			ImsMetricsUtil.calculateSourceMetricsOnError(builder, sourceObject, e.getMessage(), moduleType);
		} catch (final Throwable e) {
			LOG.error("Unexpected error occurred while parsing" + sourceObject.getPath(), e);
			ImsMetricsUtil.calculateSourceMetricsOnError(builder, sourceObject, e.getMessage(), moduleType);
		}
	}
	
	private String getDbdNameIfNotBlank(final DbdGenModel dbdGenModel, final String moduleName) {
		final String dbdName = dbdGenModel.getDbd().getName();
		return StringUtils.isNotBlank(dbdName) ? dbdName : moduleName;
	}
	
	private void collectDbdAttributes(final ModuleBuilder rootModule, final DbdGenModel dbdGenModel, final Set<String> statements) {
		final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
		final var dbd = dbdGenModel.getDbd();
		attributeMap.put(IMS_ACCESS_TYPE, dbd.getIMSAccess());
		attributeMap.put(IMS_OS_ACCESS_TYPE, dbd.getOSAccess());

		final var dataset = dbdGenModel.getDataset();
		if (dataset != null) {
			attributeMap.put(IMS_DBD_DATASET, dataset.getDD1());
		}
		final var attributeString = attributeMap.toString();
		if (statements.add(attributeString)) {
			rootModule.declareStatement(StatementType.DB_ACCESS_TYPE).setText(attributeString);
		}
	}

	private void collectSubModulesAndDependencies(final DiscoveryBuilderFromSource builder, final DbdGenModel dbdGenModel, final ModuleBuilder rootModule,
			final String rootModulePath) {
		for (final Segment segment : dbdGenModel.getSegments()) {
			final String segmentName = segment.getName();
			final var segmentModuleBuilder = builder.declareSubModule(segmentName, ModuleType.IMS_DBD_SEGMENT);
			final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
			/* we track the parent in the attributes, as a segment can be present multiple times with the same name in different DBDs,
			 * but with a different parent structure. */
			final Object parent = segment.getParent();
			if (parent != null) {
				attributeMap.put(IMS_DBD_SEGMENT_PARENT, parent.toString());
			}
			final List<String> comprtn = segment.getComprtn();
			if (comprtn != null) {
				attributeMap.put(IMS_DBD_SEGMENT_COMPRTN, comprtn.toString());
			}
			
			final ModuleFilter dbdFilter = new ModuleFilter().setPaths(rootModulePath).setTypes(ModuleType.IMS_DBD).setPhysical(true);
			final ModuleFilter dbdSegmentFilter = new ModuleFilter().setNames(segmentName).setTypes(ModuleType.IMS_DBD_SEGMENT).setContainedIn(dbdFilter);
			
			rootModule.declareDependency(RelationshipType.REFERENCES, dbdSegmentFilter, ResolutionFlag.MERGE_DUPLICATES)
			.setAttributes(attributeMap)
			.setBinding(Binding.EARLY);
			
			collectLogicalDependenciesForSegment(rootModule, segment, segmentModuleBuilder);
		}
	}
	
	private void collectLogicalDependenciesForSegment(final ModuleBuilder dbdModuleBuilder, final Segment segment,
			final ModuleBuilder segmentModuleBuilder) {
		/* collects LChild references */
		final List<Lchild> lchildStmts = segment.getStatements().stream()
				.filter(Lchild.class::isInstance)
				.map(Lchild.class::cast)
				.collect(Collectors.toList());
		for (final Lchild lchild : lchildStmts) {
			final List<String> nameProp = lchild.getName();
			if (nameProp.size() != 2) {
				dbdModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
						"Could not add logical relationship dependency because of wrong number of arguments in the LChild");
				break;
			}
			final String segmentName = nameProp.get(0);
			final String dbdName = nameProp.get(1);
			/*
			 * putting a special property on the relationship, so it can be identified as
			 * LCHILD relationship (the relationship type will be "REFERENCES")
			 */
			final ModelAttributeMap<Object> lchildAttributes = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
			lchildAttributes.put(ModelAttributeKey.IMS_REFERENCE_TYPE, ModelAttributeValue.ImsReferenceType.LCHILD);
			
			declareDependency(dbdModuleBuilder, segmentModuleBuilder, segmentName, dbdName, lchildAttributes);
		}

		/* collects virtual references identified by "SOURCE" */
		final Object propertySource = segment.getProperty("SOURCE");
		if (propertySource instanceof List) {
			final List<?> propertySourceList = (List<?>) propertySource;
			propertySourceList.stream()
					.filter(List.class::isInstance)
					.map(List.class::cast)
					.map(list -> {
						if (list.size() == 3) {
							return Tuple2.of(list.get(0).toString(), list.get(2).toString());
						}
						segmentModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
								"Could not add logical relationship dependency because of wrong number of arguments in the \"Source\" property");
						return null;
					})
					.filter(Objects::nonNull)
					.forEach(targetSegment -> {
						final String segmentName = targetSegment.a;
						final String dbdName = targetSegment.b;
						/*
						 * putting a special property on the relationship, so it can be identified as
						 * VIRTUAL relationship (the relationship type will be "REFERENCES")
						 */
						final ModelAttributeMap<Object> virtualAttribute = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
						virtualAttribute.put(ModelAttributeKey.IMS_REFERENCE_TYPE, ModelAttributeValue.ImsReferenceType.VIRTUAL);

						declareDependency(dbdModuleBuilder, segmentModuleBuilder, segmentName, dbdName, virtualAttribute);
					});
		}
	}

	private void declareDependency(final ModuleBuilder dbdModuleBuilder, final ModuleBuilder segmentModuleBuilder, final String segmentName,
			final String dbdName, final ModelAttributeMap<Object> virtualAttribute) {
		final ModuleFilter dbdFilter = new ModuleFilter().setNames(dbdName).setTypes(ModuleType.IMS_DBD);
		final ModuleFilter dbdSegmentFilter = new ModuleFilter().setNames(segmentName).setTypes(ModuleType.IMS_DBD_SEGMENT).setContainedIn(dbdFilter);

		segmentModuleBuilder.declareDependency(RelationshipType.REFERENCES, dbdSegmentFilter)
		.setBinding(Binding.EARLY)
		.setAttributes(virtualAttribute);

		dbdModuleBuilder.declareDependency(RelationshipType.REFERENCES, dbdFilter)
		.setBinding(Binding.EARLY)
		.setAttributes(virtualAttribute);
	}
}
