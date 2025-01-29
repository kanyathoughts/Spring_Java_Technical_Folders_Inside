/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.discovery.featurereport.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Describes the Discovery features used by a single Technology and Type.
 */
public class DiscoveryFeatures implements Serializable {

	private final Technology technology;
	private final Type type;

	/* === what characteristics does a module of this technology and type have? === */
	private final Set<Storage> storages = new HashSet<>();
	private final Set<Origin> origins = new HashSet<>();
	private final Set<Identification> identifications = new HashSet<>();
	private final Set<String> representations = new HashSet<>();
	private final Set<String> excelTypes = new HashSet<>();
	
	private final Set<TechnologyAndType> containedIn = new HashSet<>();
	private final Set<TechnologyAndType> contains = new HashSet<>();

	private Supported hasPath = Supported.NO;
	private Supported hasContentsPath = Supported.NO;
	private Supported hasContainedModules = Supported.NO;
	private Supported hasContainingModule = Supported.NO;
	private Supported hasSourceCode = Supported.NO; // this one maybe makes no sense
	private Supported hasLocation = Supported.NO;
	private Supported hasErrors = Supported.NO; // maybe useful
	private Supported hasAst = Supported.NO;

	/* === which metrics are supported? === */
	private Supported supportsPhysicalLines = Supported.NO;
	private Supported supportsCodeLines = Supported.NO;
	private Supported supportsCommentLines = Supported.NO;
	private Supported supportsLinesOfDeadCode = Supported.NO;
	private Supported supportsComplexity = Supported.NO;
	private Supported supportsStatements = Supported.NO;
	private Supported supportsSqlStatements = Supported.NO;
	/* ... etc. for other metrics */

	/* === what kind of (outgoing!) relationships exist? === */
	private final Map<RelationshipTo, RelationshipTo> possibleRelationships = new HashMap<>();

	public DiscoveryFeatures(Technology technology, Type type) {
		this.technology = technology;
		this.type = type;
	}

	public Supported getHasContentsPath() {
		return hasContentsPath;
	}

	public void setContentsPath(Supported hasContentsPath) {
		this.hasContentsPath = hasContentsPath;
	}
	
	public Supported getHasPath() {
		return hasPath;
	}

	public void setHasPath(Supported hasPath) {
		this.hasPath = hasPath;
	}
	
	public Supported getHasContainedModules() {
		return hasContainedModules;
	}
	
	public void setHasContainedModules(Supported hasContainedModules) {
		this.hasContainedModules = hasContainedModules;
	}
	
	public Supported getHasContainingModule() {
		return hasContainingModule;
	}
	
	public void setHasContainingModule(Supported hasContainingModule) {
		this.hasContainingModule = hasContainingModule;
	}

	public Supported getHasSourceCode() {
		return hasSourceCode;
	}

	public void setHasSourceCode(Supported hasSourceCode) {
		this.hasSourceCode = hasSourceCode;
	}

	public Supported getHasLocation() {
		return hasLocation;
	}

	public void setHasLocation(Supported hasLocation) {
		this.hasLocation = hasLocation;
	}

	public Supported getHasErrors() {
		return hasErrors;
	}

	public void setHasErrors(Supported hasErrors) {
		this.hasErrors = hasErrors;
	}
	
	public Supported getHasAst() {
		return hasAst;
	}
	
	public void setHasAst(Supported hasAst) {
		this.hasAst = hasAst;
	}

	public Supported getSupportsPhysicalLines() {
		return supportsPhysicalLines;
	}

	public void setSupportsPhysicalLines(Supported supportsPhysicalLines) {
		this.supportsPhysicalLines = supportsPhysicalLines;
	}
	
	public Supported getSupportsCodeLines() {
		return supportsCodeLines;
	}

	public void setSupportsCodeLines(Supported supportsCodeLines) {
		this.supportsCodeLines = supportsCodeLines;
	}

	public Supported getSupportsCommentLines() {
		return supportsCommentLines;
	}

	public void setSupportsCommentLines(Supported supportsCommentLines) {
		this.supportsCommentLines = supportsCommentLines;
	}

	public Supported getSupportsLinesOfDeadCode() {
		return supportsLinesOfDeadCode;
	}

	public void setSupportsLinesOfDeadCode(Supported supportsLinesOfDeadCode) {
		this.supportsLinesOfDeadCode = supportsLinesOfDeadCode;
	}

	public Supported getSupportsComplexity() {
		return supportsComplexity;
	}

	public void setSupportsComplexity(Supported supportsComplexity) {
		this.supportsComplexity = supportsComplexity;
	}

	public Supported getSupportsStatements() {
		return supportsStatements;
	}

	public void setSupportsStatements(Supported supportsStatements) {
		this.supportsStatements = supportsStatements;
	}

	public Supported getSupportsSqlStatements() {
		return supportsSqlStatements;
	}

	public void setSupportsSqlStatements(Supported supportsSqlStatements) {
		this.supportsSqlStatements = supportsSqlStatements;
	}

	public Technology getTechnology() {
		return technology;
	}

	public Type getType() {
		return type;
	}

	public Set<Storage> getStorages() {
		return storages;
	}

	public Set<Origin> getOrigins() {
		return origins;
	}

	public Set<Identification> getIdentifications() {
		return identifications;
	}

	public Set<String> getRepresentations() {
		return representations;
	}

	public Set<String> getExcelTypes() {
		return excelTypes;
	}

	public Set<RelationshipTo> getPossibleRelationships() {
		return new HashSet<>(possibleRelationships.values());
	}
	
	public Set<TechnologyAndType> getContainedIn() {
		return containedIn;
	}
	
	public Set<TechnologyAndType> getContains() {
		return contains;
	}

	public void addStorage(Storage storage) {
		storages.add(storage);
	}

	public void addOrigin(Origin origin) {
		origins.add(origin);
	}

	public void addIdentification(Identification identification) {
		identifications.add(identification);
	}

	public void addRepresentation(String representation) {
		representations.add(representation);
	}

	public void addExcelType(String excelType) {
		excelTypes.add(excelType);
	}
	
	public void addPossibleRelationship(RelationshipTo relationship) {
		if (possibleRelationships.containsKey(relationship)) {
			final RelationshipTo existing = possibleRelationships.get(relationship);
			final RelationshipTo merged = existing.withMergedAttributes(relationship.getPossibleAttributes());
			possibleRelationships.put(merged, merged);
		} else {
			possibleRelationships.put(relationship, relationship);
		}
	}
	
	public void addContains(TechnologyAndType pair) {
		contains.add(pair);
	}
	
	public void addContainedIn(TechnologyAndType pair) {
		containedIn.add(pair);
	}

	public String[] toCsv() {
		return new String[] {
				getTechnology().toString(),
				getType().toString(),
				
				getStorages().stream().map(o -> o == null ? "null" : o.toString()).sorted().collect(Collectors.joining(",")),
				getOrigins().stream().map(o -> o == null ? "null" : o.toString()).sorted().collect(Collectors.joining(",")),
				getIdentifications().stream().map(o -> o == null ? "null" : o.toString()).sorted().collect(Collectors.joining(",")),
				getRepresentations().stream().map(o -> o == null ? "null" : o.toString()).sorted().collect(Collectors.joining(",")),
				getExcelTypes().stream().map(o -> o == null ? "null" : o.toString()).sorted().collect(Collectors.joining(",")),
				
				getPossibleRelationships().stream().map(o -> o == null ? "null" : o.toString()).sorted().collect(Collectors.joining(",")),
				
				getContainedIn().stream().sorted().map(TechnologyAndType::toString).collect(Collectors.joining(",")),
				getContains().stream().sorted().map(TechnologyAndType::toString).collect(Collectors.joining(",")),
				
				getHasPath().toString(),
				getHasContainingModule().toString(),
				getHasContainedModules().toString(),
				getHasSourceCode().toString(),
				getHasLocation().toString(),
				getHasErrors().toString(),
				getHasAst().toString(),
				
				getSupportsPhysicalLines().toString(),
				getSupportsCodeLines().toString(),
				getSupportsCommentLines().toString(),
				getSupportsLinesOfDeadCode().toString(),
				getSupportsComplexity().toString(),
				getSupportsStatements().toString(),
				getSupportsSqlStatements().toString(),
		};
	}
	
	public static String[] getCsvHeaders() {
		return new String[] {
				"Technology",
				"Type",
				
				"Storage",
				"Origin",
				"Identification",
				"Representation",
				"ExcelType",
				
				"Relationships",
				
				"ContainedIn",
				"Contains",
				
				"HasPath",
				"HasContainingModule",
				"HasContainedModules",
				"HasSourceCode",
				"HasLocation",
				"HasErrors",
				"HasAst",
				
				"SupportsPhysicalLines",
				"SupportsCodeLines",
				"SupportsCommentLines",
				"SupportsLinesOfDeadCode",
				"SupportsComplexity",
				"SupportsStatements",
				"SupportsSqlStatements"
		};
	}
}
