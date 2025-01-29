/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.WRITE;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.model.ast.DeleteStatement;
import innowake.ndt.parsing.parser.basic.model.ast.GetStatement;
import innowake.ndt.parsing.parser.basic.model.ast.KillStatement;
import innowake.ndt.parsing.parser.basic.model.ast.PutStatement;
import innowake.ndt.parsing.parser.basic.model.ast.ScratchStatement;
import innowake.ndt.parsing.parser.basic.model.ast.UpdateStatement;
import org.springframework.stereotype.Component;

/**
 * Taxonomy identifier for identifying file access taxonomies based on AstModel.
 */
@Component
public class AstModelFileAccessTaxonomyIdentifier implements TaxonomyIdentifier<AstModel> {
	
	private static final Set<String> RELEVANT_READ_TYPES = new HashSet<>();
	private static final Set<String> RELEVANT_WRITE_TYPES = new HashSet<>();
	private static final Tuple2<Name, TypeName> READ_TAXONOMY;
	private static final Tuple2<Name, TypeName> WRITE_TAXONOMY;
	
	static {
		READ_TAXONOMY = TechnicalTaxonomies.tuple(READ, TypeName.FILE_ACCESS);
		WRITE_TAXONOMY = TechnicalTaxonomies.tuple(WRITE, TypeName.FILE_ACCESS);
		
		RELEVANT_READ_TYPES.add(GetStatement.class.getSimpleName());
		
		RELEVANT_WRITE_TYPES.add(DeleteStatement.class.getSimpleName());
		RELEVANT_WRITE_TYPES.add(KillStatement.class.getSimpleName());
		RELEVANT_WRITE_TYPES.add(ScratchStatement.class.getSimpleName());
		RELEVANT_WRITE_TYPES.add(UpdateStatement.class.getSimpleName());
		RELEVANT_WRITE_TYPES.add(PutStatement.class.getSimpleName());
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final AstModel model) {
		final Set<Tuple2<Name, TypeName>> identifiedTaxonomies = new HashSet<>();
		if (model instanceof BasicModel) {
			final Optional<AstNode> root = model.getRoot();
			root.ifPresent( astNode -> astNode.getChildren().stream()
				.map(AstNode::getClass)
				.map(Class::getSimpleName).forEach( name -> {
					if (RELEVANT_READ_TYPES.contains(name)) {
						identifiedTaxonomies.add(READ_TAXONOMY);
					}
					if (RELEVANT_WRITE_TYPES.contains(name)) {
						identifiedTaxonomies.add(WRITE_TAXONOMY);
					}
				})
			);
			
		}
		return new ArrayList<>(identifiedTaxonomies);
	}

}
