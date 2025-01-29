/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.cobol;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol.CobolReferenceResolver;
import innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol.DefaultCobolReferenceResolver;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolFileDefinition;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt.Organization;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Extract all the file dependencies defined in COBOL.<br>
 * Assumptions made:
 * <ul>
 *   <li>The SELECT statements get added BEFORE the FILE DEFINITIONS and the first token after
 *       the SELECT statement itself is the virtual file variable which is used a 
 *       reference throughout the cobol-file.</li>
 *   <li>The target filename is '/{DISCOVERED_FILENAME}.DAT'</li>
 * </ul>
 * The HP Cobol reference from 
 * <a href="https://support.hpe.com/hpesc/public/docDisplay?docId=emr_na-c04621339}">HP COBOL documentation</a>(4-29) also states:<br>
 * <i>If there is a file specification in an associated VALUE OF ID clause, the
 * ASSIGN clause contains the default file specification. File specification
 * components in the VALUE OF ID clause override those in the ASSIGN
 * clause. </i>
 */
public class CobolFileDependencies {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_MODULE_REPOSITORY);

	private final Map<String, ModelAttributeMap<Object>> vNameToAttributeMap = new LinkedHashMap<>();

	private final Map<String, Set<CobolFileDependency>> potentialFileResources = new LinkedHashMap<>();
	private final Map<CobolFileDependency, Binding> bindings = new LinkedHashMap<>();
	private final Map<CobolFileDependency, ModelAttributeMap<Object>> moduleToAttributeMaps = new LinkedHashMap<>();
	
	private final DefaultCobolReferenceResolver referenceResolver;
	private final List<CobolDataField> dataFields;

	/**
	 * Create a new instance with a configured instance of a {@link CobolReferenceResolver} and dataFields from a parsed cobol module.
	 * @param referenceResolver a configured {@link CobolReferenceResolver}
	 * @param dataFields a list of dataFields from an analyzed cobol file.
	 */
	public CobolFileDependencies(final DefaultCobolReferenceResolver referenceResolver, final List<CobolDataField> dataFields) {
		this.referenceResolver = referenceResolver;
		this.dataFields = dataFields;
	}

	/**
	 * Add all file references.
	 * 
	 * @param selectStmt the select-statement to add.
	 */
	public void addFilesFromSelectStatement(@Nullable final CobolSelectStmt selectStmt) {
		if (selectStmt == null || selectStmt.getChildren().size() < 2) return;
		final var indexOfFileSpec = 1;
		final List<AstNode> children = selectStmt.getChildren(); 
		var orga = selectStmt.getOrganization();
		if (orga == null) {
			/* The default organization, according to the HP Cobol documentation, is SEQUENTIAL. */
			orga = Organization.SEQUENTIAL;
		}
		final AstNode fileSpec = children.get(indexOfFileSpec);
		final String virtualFile = selectStmt.getStartToken().getNextTokenNullsafe().getText();
		final ModelAttributeMap<Object> attrMap = new ModelAttributeMap<>();
		attrMap.put(ModelAttributeKey.FILE_ACCESS_TYPE, orga);
		vNameToAttributeMap.put(virtualFile, attrMap);
		
		if (fileSpec instanceof CobolConstantReference) {
			final CobolConstantReference fileSpecConst = (CobolConstantReference) fileSpec;
			final String name = MetricsUtility.trimQuotesSpaces(fileSpecConst.getValue().toString());
			final var module = createModule(name);
			moduleToAttributeMaps.put(module, attrMap);
			addFileDependencies(virtualFile, Collections.singletonList(module));
			bindings.put(module, Binding.EARLY);
		} else if (fileSpec instanceof CobolFieldReference) {
			final Optional<CobolDataField> correspondingDataField = getDataFieldByName(fileSpec.toString());
			if (correspondingDataField.isEmpty()) {
				return;
			}
			final CobolFieldReference fileSpecRef = (CobolFieldReference) fileSpec;
			fileSpecRef.setField(correspondingDataField.get());

			final List<String> resolvedNames = referenceResolver.resolve(fileSpec);
			final List<CobolFileDependency> modules = resolvedNames.stream()
					.map(this::createModule)
					.collect(Collectors.toList());
			addFileDependencies(virtualFile, modules);
			modules.forEach(module -> {
				bindings.put(module, Binding.LATE);
				moduleToAttributeMaps.put(module, attrMap);
			});
		}
	}

	private void addFileDependencies(final String virtualFile, final List<CobolFileDependency> modules) {
		potentialFileResources.computeIfAbsent(virtualFile, k -> new HashSet<>()).addAll(modules);
	}
	
	/**
	 * Add all files from FD statements. Do this after adding all select statements, 
	 * since this will overwrite findings from FD statements in case of a 'VALUE OF ID' token 
	 * is encountered.
	 * @param fileDefinition the files resulting from the file-definitions.
	 */
	public void addFilesFromFileDefinition(@Nullable final CobolFileDefinition fileDefinition) {
		if (fileDefinition == null 
		 || fileDefinition.getName() == null 
		 || fileDefinition.getValueOfId() == null) {
			return;
		}
		final String virtualName = fileDefinition.getName();
		final CobolReference valueOfId = fileDefinition.getValueOfId(); 
		
		if (valueOfId instanceof CobolConstantReference) {
			final CobolConstantReference valueOfIdConst = (CobolConstantReference) valueOfId;
			final String strValueOfId = MetricsUtility.trimQuotesSpaces(valueOfIdConst.getValue().toString());
			if ( ! strValueOfId.isEmpty()) {
				final var module = createModule(strValueOfId);
				addFileDependencies(virtualName, Collections.singletonList(module));
				bindings.put(module, Binding.EARLY);
				moduleToAttributeMaps.put(module, getAttributeMap(virtualName));
			}
		} else if (valueOfId instanceof CobolFieldReference) {
			final Optional<CobolDataField> correspondingDataField = getDataFieldByName(valueOfId.toString());
			if (correspondingDataField.isPresent()) {
				final CobolDataField targetField = correspondingDataField.get();
				final CobolFieldReference fieldRef = (CobolFieldReference) valueOfId;
				fieldRef.setField(targetField);

				final List<String> resolvedNames = referenceResolver.resolve(fieldRef);
				final List<CobolFileDependency> modules = resolvedNames.stream()
						.map(this::createModule)
						.collect(Collectors.toList());
				addFileDependencies(virtualName, modules);
				modules.forEach(module -> { 
					bindings.put(module,  Binding.LATE);
					moduleToAttributeMaps.put(module, getAttributeMap(virtualName));
				});
			}
			
		}
	}
	
	/**
	 * Get all FileResources that have been found in the select / FD statements.
	 * @return a list of all Modules 
	 */
	public List<CobolFileDependency> getFileResources() {
		return potentialFileResources.values().stream().flatMap(Collection::stream).collect(Collectors.toList());
	}
	
	/**
	 * Retrieve the attribute corresponding to a file definition.
	 *
	 * @param module the module for which to retrieve the attributes
	 * @return a map containing file access types defined by the organization of a file in cobol
	 */
	public ModelAttributeMap<Object> getAttributes(final CobolFileDependency module) {
		if ( ! moduleToAttributeMaps.containsKey(module)) {
			/* Return an empty map.*/
			return new ModelAttributeMap<>();
		}
		return moduleToAttributeMaps.get(module);
	}

	/**
	 * Return a binding for a particular model-artifact that has
	 * been created here in the process.
	 * 
	 * @param module a model artifact obtained from {@link #getFileResources()}
	 * @return a binding associated with this model artifact, see {@link Binding}
	 */
	public Binding getBinding(final CobolFileDependency module) {
		return bindings.get(module);
	}
	
	/**
	 * Return the corresponding CobolDataField for a dataFieldName which is searched
	 * through all available datafields.
	 *
	 * @param dataFieldName a data field name as defined in the working section
	 * @return an optional {@link CobolDataField}
	 */
	private Optional<CobolDataField> getDataFieldByName(final String dataFieldName) {
		final List<CobolDataField> dataFieldsFound = new ArrayList<>();
		for (final CobolDataField dataField : dataFields) {
			if (dataField.getName() != null && dataField.getName().equals(dataFieldName)) {
				dataFieldsFound.add(dataField);
			}
		}
		if (dataFieldsFound.size() == 1) {
			return Optional.of(dataFieldsFound.get(0));
		} else if (dataFieldsFound.size() > 1) {
			LOG.error(() -> "COBOL File resolving: Found multiple datafields for one dataFieldName. Will use the first occurence: " + dataFieldName);
			return Optional.of(dataFieldsFound.get(0));
		}
		return Optional.empty();
	}
	
	private CobolFileDependency createModule(final String name) {
		return new CobolFileDependency(name, ModuleType.RESOURCE_FILE);
	}
	
	private ModelAttributeMap<Object> getAttributeMap(final String virtualFileName) {
		return vNameToAttributeMap.computeIfAbsent(virtualFileName, str -> new ModelAttributeMap<Object>());
	}
	
	public static class CobolFileDependency {

		private final String name;
		private final ModuleType moduleType;

		public CobolFileDependency(final String name, final ModuleType moduleType) {
			this.name = name;
			this.moduleType = moduleType;
		}

		public String getName() {
			return name;
		}

		public ModuleType getModuleType() {
			return moduleType;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			final CobolFileDependency other = (CobolFileDependency) obj;
			return name.equals(other.name) && moduleType == other.moduleType;
		}

		@Override
		public int hashCode() {
			return Objects.hash(name, moduleType);
		}
	}

}
