/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationCategory.DatabaseAnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Technology;

/**
 * Annotation identifier for database statements. 
 * 
 * The resulting annotations have the following attributes:
 * <ul>
 * <li>Type: {@link AnnotationType#DATABASE}
 * <li>Name: System identified Database Query
 */
public class DatabaseAnnotationIdentifier implements AnnotationIdentifier {
	
	private static final String ANNOTATION_NAME = "System identified Database Query";
	
	private final AstNodeCollector collector;
	
	private static final Map<String, DatabaseAnnotationCategory> NODE_TYPE_TO_CATEGORY_MAP = new HashMap<>();

	static {
		
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlSelect", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlFetch", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("ReadStmt", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("ReadWithoutDescriptorStmt", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("GetStmt", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("FindStmt", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("HistogramStmt", DatabaseAnnotationCategory.READ);
		NODE_TYPE_TO_CATEGORY_MAP.put("SelectStmt", DatabaseAnnotationCategory.READ);

		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlInsert", DatabaseAnnotationCategory.WRITE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlUpdate", DatabaseAnnotationCategory.WRITE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDelete", DatabaseAnnotationCategory.WRITE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlCommit", DatabaseAnnotationCategory.WRITE);
		NODE_TYPE_TO_CATEGORY_MAP.put("StoreStmt", DatabaseAnnotationCategory.WRITE);
		NODE_TYPE_TO_CATEGORY_MAP.put("DeleteStmt", DatabaseAnnotationCategory.WRITE);
		NODE_TYPE_TO_CATEGORY_MAP.put("UpdateStmt", DatabaseAnnotationCategory.WRITE);

		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDeclareTempTable", DatabaseAnnotationCategory.DECLARE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDeclareTable", DatabaseAnnotationCategory.DECLARE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDeclareCursor", DatabaseAnnotationCategory.DECLARE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDeclareSchema", DatabaseAnnotationCategory.DECLARE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDeclareAlias", DatabaseAnnotationCategory.DECLARE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlDeclareTransaction", DatabaseAnnotationCategory.DECLARE);
		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlBeginDeclareSection", DatabaseAnnotationCategory.DECLARE);

		NODE_TYPE_TO_CATEGORY_MAP.put("ExecSqlClose", DatabaseAnnotationCategory.CLOSE);
	}

	/**
	 * Creates a new database identifier.
	 */
	public DatabaseAnnotationIdentifier() {
		collector = new AstNodeCollector(node -> node.getSuperTypes().contains(AstNodeUtils.DATABASE_ACCESS_STATEMENT));
	}

	@Override
	public List<AnnotationPojoTemplate> identify(final AstNodePojo root, final Map<String, Long> annotationCategoryMap, final Technology technology,
			Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		final List<AnnotationPojoTemplate> allAnnotations = collector.allDeep(root).stream().map(node -> AnnotationCreator.create(node, ANNOTATION_NAME,
				AnnotationType.DATABASE, annotationCategoryMap.get(getDatabaseAnnotationCategory(node).getName()))).collect(Collectors.toList());
		return AnnotationMerger.merge(allAnnotations);
	}

	private DatabaseAnnotationCategory getDatabaseAnnotationCategory(final AstNodePojo node) {
		 String nodeType = node.getType();
		return NODE_TYPE_TO_CATEGORY_MAP.getOrDefault(nodeType, DatabaseAnnotationCategory.UNKNOWN);
	}
}
