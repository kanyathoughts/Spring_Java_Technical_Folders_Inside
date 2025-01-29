/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.datadictionary.impl.CicsDataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.CobolDataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.DataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.NaturalDataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.Pl1DataDictionaryIdentifier;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.DataFieldFormat;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.ndt.cobol.parser.ast.model.CobolDataField.Usage;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.Format;

/**
 * Service for AST Node requests.
 */
@Service
@Transactional("postgres")
public class AstNodeService {

	private static final String FIELD_DEFINITION = "FieldDefinition";
	private static final String FIELD_REFERENCE = "FieldReference";
	private static final String INCLUSION_NODE = "INCLUSION_NODE";
	private static final Set<Type> INCLUDE_TYPES = Sets.newHashSet(Type.COPYBOOK, Type.CONTROLCARD, Type.COPYCODE, Type.COPYLIB, Type.COPYPROC, Type.GDA,
		Type.INCLUDE, Type.INLINE_PROC, Type.LDA, Type.PDA,Type.PROC);

	@Autowired
	private DataDictionaryService dataDictionaryService;
	
	@Autowired
	private MiningDataCoreService core;

	/**
	 * Returns the format of data field selected in the code editor.
	 * 
	 * @param projectId  the ID of the project.
	 * @param moduleId the ID of the module.
	 * @param offset the offset of selected text.
	 * @param assembled if the field is selected in assembled mode
	 * @throws MiningEntityNotFoundException if selection is not valid.
	 * @return {@linkplain DataFieldFormat} object with details of the selected field.
	 */
	public DataFieldFormat getFormatIfSelectionIsValid(final EntityId projectId, final EntityId moduleId, final int offset, final boolean assembled) {
		final List<AstNodePojo> nodesInDb;

		final ModuleLightweightPojo module = core.moduleService.findAnyModuleLightweight(b -> b.byId(moduleId))
				.orElseThrow(() -> new IllegalStateException("Module with id " + moduleId + " does not exist."));
		if (module.getType() == Type.COPYBOOK) {
			nodesInDb = getAstNodesForCopyBook(projectId, moduleId, offset).stream()
					.filter(node -> (node.getSuperTypes().contains(FIELD_REFERENCE)
					|| node.getSuperTypes().contains(FIELD_DEFINITION))).toList();
		} else if (assembled) {
			nodesInDb = core.astService.find(q -> q.ofModule(moduleId)
					.withAssembledOffset(Comperator.LESSER_OR_EQUAL, offset)
					.withAssembledEndOffset(Comperator.GREATER_OR_EQUAL, offset)
					.withSuperTypes(FIELD_REFERENCE, FIELD_DEFINITION));
		} else {
			nodesInDb = core.astService.find(q -> q.ofModule(moduleId)
					.withIncludedModule(null)
					.withRetracedOffset(Comperator.LESSER_OR_EQUAL, offset)
					.withRetracedEndOffset(Comperator.GREATER_OR_EQUAL, offset)
					.withSuperTypes(FIELD_DEFINITION, FIELD_REFERENCE));
		}
		
		if (! nodesInDb.isEmpty()) {
			final AstNodePojo node = nodesInDb.get(0);
			if (node.getSuperTypes().contains(FIELD_DEFINITION)) {
				return getFormatFromFieldDefinition(moduleId, offset, module, node, assembled);
			} else if (node.getSuperTypes().contains(FIELD_REFERENCE)) {
				final List<AstRelationshipPojo> refersToReferences = node.getOutgoingRelations().stream()
						.filter(r -> r.getType().equals(AstRelationshipType.REFERS)).collect(Collectors.toList());
				if ( ! refersToReferences.isEmpty()) {
					return getFieldDefinitionFromReferences(refersToReferences, node, module, assembled);
				}
			}
		}
		throw new MiningEntityNotFoundException(AstNodePojo.class, String.format("at module %s, offset %d.", moduleId, offset));
	}

	private DataFieldFormat getFormatFromFieldDefinition(final EntityId moduleId, final int offset, final ModuleLightweightPojo module, final AstNodePojo node, 
			final boolean assembled) {
		final ModulePojo copybook = getCopybookModule(node, node.getParent().orElse(null));
		if (copybook != null) {
			return populateDataFieldFormat(copybook.identity(), node, getDataDictionaryIdFromAstNode(node, copybook.identity()), module, assembled);
		}
		
		return checkForConstraintVoilation(moduleId, offset, module, node, assembled);
	}

	@Nullable
	private ModulePojo getCopybookModule(final AstNodePojo node, @Nullable final AstNodePojo parentNode) {
		AstNodePojo parent = parentNode;
		ModulePojo copybook = null;
		while (parent != null && parent != node) {
			if (parent.getSuperTypes().contains(INCLUSION_NODE)) {
				copybook = parent.getIncludedModule().flatMap(id -> core.moduleService.findAnyModule(q -> q.byId(id)))
							.orElseThrow(() -> new NoSuchElementException("InclusionCalleeModuleId of parent must not be null"));
				break;
			}
			parent = parent.getParent().orElse(null);
		}
		return copybook;
	}

	private DataFieldFormat checkForConstraintVoilation(final EntityId moduleId, final int offset, final ModuleLightweightPojo module, final AstNodePojo node,
			final boolean assembled) {
		final ModuleLocation location = getFieldNameLocation(node, moduleId, assembled);
		if (offset < location.getOffset() || offset > location.getOffset() + location.getLength()) {
			throw new ConstraintViolationException("Not a data field. Place cursor in data field name to proceed");
		} else {
			return populateDataFieldFormat(moduleId, node, getDataDictionaryIdFromAstNode(node, moduleId), module, assembled);
		}
	}
	
	private DataFieldFormat getFieldDefinitionFromReferences(final List<AstRelationshipPojo> refersToReferences, final AstNodePojo node,
			final ModuleLightweightPojo module, final boolean assembled) {
		if (refersToReferences.size() == 1) {
			final AstRelationshipPojo refersTo = refersToReferences.get(0);
			final AstNodePojo fieldDefinition = refersTo.getDstNode(); 
			AstNodePojo parent = fieldDefinition.getParent().orElse(null);
			ModulePojo copybook = null;
			while (parent != null && parent != node) {
				if (parent.getSuperTypes().contains(INCLUSION_NODE)) {
					final var id = parent.getIncludedModule().orElseThrow(() -> new IllegalStateException("InclusionCalleeModuleId of parent must not be null"));
					copybook = core.moduleService.getModule(id);
					break;
				}
				parent = parent.getParent().orElse(null);
			}

			final EntityId ddeModuleId = copybook != null ? copybook.identity() : module.identity();
			return populateDataFieldFormat(ddeModuleId, fieldDefinition, getDataDictionaryIdFromAstNode(fieldDefinition, ddeModuleId), module, assembled);
		} else {
			throw new IllegalStateException(String.format("Field Reference should have 1 outgoing RefersTo edge but has %d edges.", refersToReferences.size()));
		}
	}

	private DataFieldFormat populateDataFieldFormat(final EntityId moduleId, final AstNodePojo datafield, @Nullable final Long dataDictionaryEntryId,
			final ModuleLightweightPojo module, final boolean assembled) {
		final ModuleLocation location = assertNotNull(getFieldNameLocation(datafield, moduleId, assembled));
		final int offset = assertNotNull(location.getOffset());
		final int length = assertNotNull(location.getLength());
		final Map<String, Object> properties = datafield.getProperties();
		final String name = (String) properties.get("name");
		String usage = null;
		final String type = datafield.getType();
		if (type != null && type.equalsIgnoreCase("CobolDataField")) {
			final String comp = (String) properties.get("comp");
			usage = comp == null ? Usage.DISPLAY.toString() : comp;
		}
		final String languageType = (String) properties.get(Format.PROPERTY_LANGUAGE_TYPE);
		final Integer byteLength = (Integer) properties.get(Format.PROPERTY_BYTE_LENGTH);
		final DefinedLocation definedLocation = getDefinedLocation(datafield, module);
		return new DataFieldFormat(name, moduleId, new ModuleLocation(offset, length), languageType, byteLength, dataDictionaryEntryId, usage,
				definedLocation);
	}

	private DefinedLocation getDefinedLocation(final AstNodePojo node, final ModuleLightweightPojo module) {
		final DataDictionaryIdentifier identifier;
		switch (module.getTechnology()) {
			case COBOL :
				identifier = new CobolDataDictionaryIdentifier(core, module.identity(), node, Optional.empty());
				break;
			case PL1 :
				identifier = new Pl1DataDictionaryIdentifier(core, module, node);
				break;
			case NATURAL :
				identifier = new NaturalDataDictionaryIdentifier(core, module, node);
				break;
			case CICS :
				identifier = new CicsDataDictionaryIdentifier(core, module.identity(), node);
				break;
			default:
				if (INCLUDE_TYPES.contains(module.getType())) {
					return DefinedLocation.COPYBOOK;
				} else {
					return DefinedLocation.PROGRAM;
				}
		}
		final DataDictionaryPojoPrototype prototype = new DataDictionaryPojoPrototype();
		identifier.setDefinedLocation(node.getIncludedModule().orElse(null), prototype, node);
		return Optional.ofNullable(prototype.definedLocation.getNonNull()).orElse(DefinedLocation.PROGRAM);
	}
	
	private ModuleLocation getFieldNameLocation(final AstNodePojo node, final EntityId moduleId, final boolean assembled) {
		final String nodeSourceText;
		final AstNodeLocation nodeLocation = node.getLocation();
		final String label = node.getLabel();
		final Integer nodeOffset;
		if (assembled) {
			nodeOffset = nodeLocation.getAssembledOffset().orElseThrow();
		} else {
			nodeOffset = nodeLocation.getRetracedOffset().orElseThrow();
		}

		if (StringUtils.containsWhitespace(label)) {
			nodeSourceText = core.moduleService.getContentSubstring(moduleId, nodeOffset, nodeLocation.getRetracedLength().orElseThrow());
		} else {
			nodeSourceText = label;
		}
		final String fieldName = (String) node.getProperties().get(FieldDefinition.PROPERTY_NAME);
		final int relativeFieldNameIndex = nodeSourceText.indexOf(fieldName);
		final int fieldNameOffset = nodeOffset + relativeFieldNameIndex;
		return new ModuleLocation(fieldNameOffset, fieldName.length());
	}
	
	@Nullable
	private Long getDataDictionaryIdFromAstNode(final AstNodePojo astNode, final EntityId moduleId) {
		final String name = (String) assertNotNull(astNode.getProperties().get("name"), "FieldDefinition AstNode should have name property set with data field name");
		final AstNodeLocation location = astNode.getLocation();
		final Integer offset = location.getRetracedOffset().orElseThrow(() -> new IllegalStateException("AstNode should have module location retraced offset"));
		final Integer length = location.getRetracedLength().orElseThrow(() -> new IllegalStateException("AstNode should have module location retraced length"));
		final List<DataDictionaryPojo> ddeId = dataDictionaryService.find(q -> q.ofModule(moduleId).withLocation(new ModuleLocation(offset, length), true).withName(name));
		return ! ddeId.isEmpty() ? ddeId.get(0).getId() : null;
	}
	
	private List<AstNodePojo> getAstNodesForCopyBook(final EntityId projectId, final EntityId moduleId, final Integer offset) {
		final ModuleLightweightPojo module = core.moduleService.findAnyModuleLightweight(q -> q.ofProject(projectId).byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId.toString() + " in project: " + projectId));
		final List<UUID> referencingModules = core.moduleService.findRelatedModules(module.identity(), RelationshipType.INCLUDES, RelationshipDirection.IN);
		if (referencingModules.isEmpty()) {
			throw new IllegalStateException("Can't create Data Dictionary Entry for this Copybook because it is not included in any Programs!");
		}
		return core.astService.find(q -> q.ofModules(referencingModules)
										.withIncludedModule(moduleId)
										.withRetracedOffset(Comperator.LESSER_OR_EQUAL, offset)
										.withRetracedEndOffset(Comperator.GREATER_OR_EQUAL, offset));
	}
}
