/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.bcss;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.ContributorParameters;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.io.sourceobject.SourceObjectDao;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.data.model.discovery.ModelDependency.Binding;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider.Pl1ParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.io.SourceObject;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.pl1parser.ast.model.Pl1Model;
import innowake.ndt.pl1parser.ast.model.expression.BaseExpression;
import innowake.ndt.pl1parser.ast.model.expression.ConstantExpression;
import innowake.ndt.pl1parser.ast.model.reference.BaseReference;
import innowake.ndt.pl1parser.ast.model.reference.BasicReference;
import innowake.ndt.pl1parser.ast.model.reference.FieldReference;
import innowake.ndt.pl1parser.ast.model.reference.QualifiedReference;
import innowake.ndt.pl1parser.ast.model.statement.AssignmentStatement;

/**
 * Contributor for PL/1 --[Calls]--> MFS dependency using BCSS specific implementation.
 */
public class Pl1MfsCallContributor implements MetricsContributor {

	private final Pl1ParseResultProvider parseResultProvider;
	private final ContributorParameters parameters;
	private final SourceObjectDao sourceObjectDao;
	private static final  String MOD_FIELD_NAME = "$PROFIL.PGM_MOD_NAM";
	private static final String MFS_CALL_TYPE = "IMS_TM";
	
	/** 
	 * Constructor
	 * 
	 * @param parseResultProvider the {@link Pl1ParseResultProvider}
	 * @param parameters the {@link ContributorParameters}
	 * @param sourceObjectDao the {@link SourceObjectDao}
	 */
	public Pl1MfsCallContributor(final Pl1ParseResultProvider parseResultProvider, final ContributorParameters parameters,
								 final SourceObjectDao sourceObjectDao) {
		this.parseResultProvider = parseResultProvider;
		this.parameters = parameters;
		this.sourceObjectDao = sourceObjectDao;
	}

	@Override
	public void calculateDependentMetrics(final ModelArtifact artifact) throws DiscoveryException {
		final Pl1ParseResult parseResult = parseResultProvider.getParseResult(getSourceObject(artifact));
		final Pl1Model pl1Model = parseResult.getProgramModel();
		final Optional<AstNode> root = pl1Model.getRoot();
		if( ! root.isPresent()) {
			return;
		}
		final List<ReferencedAssignment> referencedAssignments = getFieldAssignmentValue(root.get());
		final Set<ModelArtifact> mfsModules = new HashSet<>();
		
		for(final ReferencedAssignment referencedAssignment: referencedAssignments) {
			final String mapName = referencedAssignment.name;
			final Optional<ModelArtifact> mfsMod = parameters.repo.getEntry(artifact, mapName, ResolveTarget.IMS_MFS_MOD);
			final ModelArtifact mfsModTarget;
			
			if (mfsMod.isPresent()) {
				mfsModTarget = assertNotNull(mfsMod.get());
				mfsModules.add(assertNotNull(mfsMod.get().getParentModule()));
			} else {
				mfsModTarget = new ModelArtifact()
						.setIdentification(Identification.MISSING)
						.setName(mapName)
						.setType(ResolveTarget.IMS_MFS_MOD)
						.validate();
			}

			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.CALL_TYPE, MFS_CALL_TYPE);

			artifact.addDependency(new ModelDependency()
					.setTarget(mfsModTarget)
					.setBinding(Binding.LATE)
					.setLocation(new ModuleLocation(referencedAssignment.startOffset, referencedAssignment.offsetLength))
					.setAttributes(attributes)
					.validate());
		}
		mfsModules.forEach(targetModule -> addDependency(artifact, targetModule));
	}

	private void addDependency(final ModelArtifact artifact, final ModelArtifact targetModule) {
		artifact.addDependency(new ModelDependency()
				.setTarget(targetModule)
				.setBinding(Binding.LATE)
				.validate());
	}

	private SourceObject getSourceObject(final ModelArtifact entry) throws DiscoveryException {
		final String path = entry.getPath().orElseThrow(() ->
				new DiscoveryException("Entry file is not present."));
		return assertNotNull(sourceObjectDao.findByPath(parameters.projectId, path));
	}
	
	private List<ReferencedAssignment> getFieldAssignmentValue(final AstNode root) {

		final List<ReferencedAssignment> refMap = new ArrayList<>();
		final List<AssignmentStatement> assignments = root
				.getChildrenDeep(AssignmentStatement.class).stream().collect(Collectors.toList());

		for (final AssignmentStatement assignment : assignments) {

			for (BaseReference ref : assignment.getReferences()) {
				final String assignmentFieldName;
				ref = ref instanceof FieldReference ? ((FieldReference) ref).getReference() : ref;
				if (ref instanceof QualifiedReference) {
					assignmentFieldName = assembleQualifiedReference((QualifiedReference) ref);
				} else {
					assignmentFieldName = ref.getIdentifier();
				}

				if (MOD_FIELD_NAME.equals(assignmentFieldName)) {
					final BaseExpression valueExpression = assignment.getExpression();
					if (valueExpression instanceof ConstantExpression) {
						final String value = ((ConstantExpression) valueExpression).toString().trim();
						final ReferencedAssignment referencedAssignment = new ReferencedAssignment();
						
						referencedAssignment.name = value.substring(1, value.length() - 1).trim();
						referencedAssignment.startOffset = valueExpression.getStartOffset();
						referencedAssignment.offsetLength = valueExpression.getLength();
						refMap.add(referencedAssignment);
					}
				}
			}
		}
		
		return refMap;
	}
	
	@Nullable
	private String assembleQualifiedReference(final QualifiedReference qualifiedReference) {
		final BaseReference locatorReference = qualifiedReference.getLocatorReference();
		final BaseReference basedReference = qualifiedReference.getBasedReference();
		if ( ! ((basedReference instanceof QualifiedReference || basedReference instanceof BasicReference) && locatorReference instanceof BasicReference)) {
			return null;
		}
		
		final StringBuilder sb = new StringBuilder(50);
		sb.append(((BasicReference) locatorReference).getIdentifier()).append(".");
		if (basedReference instanceof QualifiedReference) {
			sb.append(assembleQualifiedReference((QualifiedReference) basedReference));
		} else {
			sb.append(((BasicReference) basedReference).getIdentifier());
		}
		return sb.toString();
	}

	private static class ReferencedAssignment {
		String name;
		Integer startOffset;
		Integer offsetLength;
	}
}
