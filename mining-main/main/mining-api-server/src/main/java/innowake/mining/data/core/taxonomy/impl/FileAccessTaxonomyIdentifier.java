/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.WRITE;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.ndt.core.parsing.ast.model.statement.FileAccessStatement;
import innowake.ndt.core.parsing.ast.model.statement.FileAccessStatement.FileAccessType;
import org.springframework.stereotype.Component;

/**
 * TaxonomyPojo identifier for identifying file access taxonomies.
 */
@Component
public class FileAccessTaxonomyIdentifier implements TaxonomyIdentifier<AstNodePojo> {
	
	private static final Logger LOG = LoggerFactory.getLogger(FileAccessTaxonomyIdentifier.class);
	private static final Tuple2<Name, TypeName> READ_TAXONOMY;
	private static final Tuple2<Name, TypeName> WRITE_TAXONOMY;
	
	static {
		READ_TAXONOMY = TechnicalTaxonomies.tuple(READ, TypeName.FILE_ACCESS);
		WRITE_TAXONOMY = TechnicalTaxonomies.tuple(WRITE, TypeName.FILE_ACCESS);
	}
	
	private final AstNodeCollector collector;
	
	/**
	 * Creates a new file access TaxonomyPojo identifier.
	 */
	public FileAccessTaxonomyIdentifier() {
		collector = new AstNodeCollector(node -> node.getSuperTypes().contains(AstNodeUtils.FILE_ACCESS_STATEMENT));
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final AstNodePojo rootNode) {
		final Set<Tuple2<Name, TypeName>> result = new HashSet<>();
		collector.allDeep(rootNode).stream().forEach(identifiedNode -> {
			final Object accessType = identifiedNode.getProperties().get(FileAccessStatement.PROPERTY_ACCESS_TYPE);
			if (! (accessType instanceof String)) {
				LOG.warn(() -> String.format("File access type missing or invalid for Module %s.", identifiedNode.getModule()));
				return;
			}
			final FileAccessType fileAccessType = FileAccessType.valueOf((String) accessType);
			switch (fileAccessType) {
				case READ:
					result.add(READ_TAXONOMY);
					break;
				case WRITE:
					result.add(WRITE_TAXONOMY);
					break;
				default:
					LOG.error(() -> String.format("Unsupported file access type %s for Module %s.", fileAccessType.name(), identifiedNode.getModule()));
					return;
			}
		});
		return new ArrayList<>(result);
	}

}
