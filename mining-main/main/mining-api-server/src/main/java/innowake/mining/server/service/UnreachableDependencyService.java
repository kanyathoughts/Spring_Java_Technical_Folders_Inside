/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.Logging;
import innowake.mining.server.event.AnnotationCreatedEvent;
import innowake.mining.server.event.AnnotationDeletedEvent;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.event.AnnotationUpdatedEvent;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.WorkingState;

/**
 * Service class for detecting and marking unreachable dependencies.
 */
@Service
@Transactional("postgres")
public class UnreachableDependencyService {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.SERVICE);
	
	private static final String MISSING_ANNOTATION_LOCATION_MESSAGE = "Missing annotation location information [annotationId=%s] [%s]";
	
	private static final String DEAD_CODE_ANNOTATION_NAME = "Dead Code Candidate [System Identified]";
	
	private static final int PAGE_LIMIT = 1000;
	
	private static final List<RelationshipType> RELEVANT_RELATIONSHIP_TYPES = List.of(
			RelationshipType.CALLS,
			RelationshipType.ACCESSES,
			RelationshipType.INCLUDES,
			RelationshipType.REFERENCES);
	
	private final ApplicationEventPublisher eventPublisher;
	
	private final AnnotationService annotationService;
	
	private final ModuleService moduleService;
	
	private final AstService astService;
	
	/**
	 * Instantiates the instance of the {@link UnreachableDependencyService} with required dependencies.
	 * 
	 * @param eventPublisher the event publisher.
	 * @param annotationService the annotation service.
	 * @param moduleService the module service.
	 * @param astService the AST node service.
	 */
	public UnreachableDependencyService(
			final ApplicationEventPublisher eventPublisher,
			final AnnotationService annotationService,
			final ModuleService moduleService,
			final AstService astService) {
		this.eventPublisher = eventPublisher;
		this.annotationService = annotationService;
		this.moduleService = moduleService;
		this.astService = astService;
	}

	/**
	 * Creates a Dead-Code {@link AnnotationPojo} for each continuous block of dead code:
	 *  {@code 
	 *  IDENTIFICATION DIVISION. 
	 *  PROGRAM-ID. A. 
	 *  ENVIRONMENT DIVISION. 
	 *  DATA DIVISION.
	 *  PROCEDURE DIVISION.
	 *  L1.
	 *      DISPLAY 'ALIVE'.
	 *      PERFORM L3.
	 *      GOBACK.
	 *  L2.							<
	 *      DISPLAY 'DEAD'.			< creates one annotation for this continuous block
	 *      DISPLAY 'DEAD2'.		<
	 *  L3.
	 *      DISPLAY 'ALIVE'.
	 *  }
	 *
	 * @param projectId The id of the project to identify the annotations in
	 * @param moduleId The id of the module to identify the annotations on
	 */
	public void createDeadCodeAnnotations(final EntityId projectId, final EntityId moduleId) {
		final Set<AstNodePojo> unreferencedTopLevelBlockNodes = findUnreferencedTopLevelBlockNodes(moduleId);
		/* Create an annotation for each block */
		for (final AstNodePojo astNode : unreferencedTopLevelBlockNodes) {

			/* We need to consider the found AstNode and all it's children */
			final List<AstNodePojo> potentialList = new ArrayList<>(AstNodeUtils.getChildrenDeep(astNode, "Statement"));
			potentialList.add(astNode);

			/* 1 Dead Code Block stretches from the lowest offset found between the unreferencedNode and its children to the highest offset + length found */
			final OptionalInt lowestStartOffset = potentialList.stream()
					.map(AstNodePojo::getLocation)
					.filter(Objects::nonNull)

					/* We need to check the lines within the same file with unassembled content and below check will validate that.
					 * The retraced location should be equal to root relative location of node. */
					.filter(moduleLocation -> Objects.equals(moduleLocation.getRootRelativeOffset(), moduleLocation.getRetracedOffset())
							&& Objects.equals(moduleLocation.getRootRelativeLength(), moduleLocation.getRetracedLength()))
					.map(AstNodeLocation::getRootRelativeOffset).filter(Optional::isPresent).mapToInt(Optional::get)
					.min();

			final OptionalInt highestEndOffset = potentialList.stream()
					.map(AstNodePojo::getLocation)
					.filter(Objects::nonNull)

					/* We need to check the lines within the same file with unassembled content and below check will validate that.
					 * The retraced location should be equal to root relative location of node. */
					.filter(moduleLocation -> Objects.equals(moduleLocation.getRootRelativeOffset(), moduleLocation.getRetracedOffset())
							&& Objects.equals(moduleLocation.getRootRelativeLength(), moduleLocation.getRetracedLength()))
					.mapToInt(location -> {
						final Integer offset = location.getRootRelativeOffset().orElseThrow();
						final Integer length = location.getRootRelativeLength().orElseThrow();

						if (offset == null || length == null || offset.intValue() + length.intValue() == 0) {
							throw new IllegalStateException("Offset or length is null or their sum is zero.");
						}

						return offset.intValue() + length.intValue();
					})
					.max();

			if (lowestStartOffset.isEmpty() || highestEndOffset.isEmpty()) {
				continue;
			}

			if ( ! hasExistingDeadCodeAnnotation(moduleId, lowestStartOffset.getAsInt(), highestEndOffset.getAsInt() - lowestStartOffset.getAsInt())) {
				/* Create annotation for each */
				createDeadCodeAnnotation(projectId, moduleId, lowestStartOffset.getAsInt(), highestEndOffset.getAsInt());
			}
		}
	}
	
	/**
	 * Retrieve the collection of top-level unreferenced block nodes.  Unreferenced block nodes that are contained by other
	 * unreferenced block nodes will be ignored as they are redundant.
	 *
	 * @param moduleId the module to retrieve the unreferenced block nodes for.
	 * @return a set of unreferenced block nodes
	 */
	private Set<AstNodePojo> findUnreferencedTopLevelBlockNodes(final EntityId moduleId) {
		final List<AstNodePojo> unreferencedBlockNodes = astService.find(q -> q.ofModule(moduleId)
																	.withSuperTypes(AstNodeUtils.CFG_COLLAPSIBLE_NODE)
																	.withRelationshipCount(AstRelationshipType.FLOW, RelationshipDirection.IN, Comperator.EQUAL, 0)
																	.withModuleRelationshipCount(AstModuleRelationshipType.CONTROL_FLOW_TERMINALS, Comperator.EQUAL, 0));
		final Set<AstNodePojo> unreferencedTopLevelBlockNodes = new HashSet<>(unreferencedBlockNodes);
		for (final AstNodePojo node : unreferencedBlockNodes) {
			AstNodePojo parent = node.getParent().orElse(null);
			while (parent != null) {
				parent = parent.getParent().orElse(null);
				if (unreferencedBlockNodes.contains(parent)) {
					unreferencedTopLevelBlockNodes.remove(node);
				}
			}
		}
		
		return unreferencedTopLevelBlockNodes;
	}
	
	/** 
	 * Method to set the field fromDeadCode to true for reference in the given project and module where they are called originating in a deadCode
	 *
	 * @param projectId the project ID of the Annotation and Reference
	 * @param moduleId the module ID of the Annotation and Reference
	 */
	public void setDeadCodeForModule(final EntityId projectId, final EntityId moduleId) {
		moduleService.setFromDeadCode(q -> q.ofModuleInDirection(moduleId, RelationshipDirection.OUT), false);
		
		final List<AnnotationPojo> annotationList = annotationService.find(q -> 
																			q.ofProject(projectId)
																			.ofModule(moduleId)
																			.withType(AnnotationType.DEAD_CODE));
		annotationList
			.stream()
			.filter(annotation -> annotation.getOffset().isEmpty() || annotation.getLength().isEmpty())
			.forEach(annotation -> LOG.error(MISSING_ANNOTATION_LOCATION_MESSAGE, 
					annotation.identity(), 
					annotation.getLocation()));
		
		LOG.debug(() -> String.format("Setting dead code for %d annotations for module %s", Integer.valueOf(annotationList.size()), moduleId));
		for (final AnnotationPojo annotation : annotationList.stream().filter(annotation -> 
																				annotation.getOffset().isPresent() && 
																				annotation.getLength().isPresent()).toList()) {
			moduleService.setFromDeadCode(q -> q.ofProject(projectId)
												.ofModuleInDirection(annotation.getModule(), RelationshipDirection.OUT)
												.withTypes(RELEVANT_RELATIONSHIP_TYPES)
												.withSourceLocationInRange(new ModuleLocation(
															annotation.getOffset().get(),
															annotation.getLength().get())),
			true);
		}
	}

	/** 
	 * Method to set the field fromDeadCode to true for reference in the given project where they are called originating in a deadCode
	 *
	 * @param projectId the project ID of the Annotation and Reference
	 */
	public void setDeadCodeForProject(final EntityId projectId) {
		moduleService.setFromDeadCode(q -> q.ofProject(projectId), false);
		
		Pagination pagination = new Pagination(PAGE_LIMIT);
		int size;
		do {
			final Paged<AnnotationPojo> paged = annotationService.find(pagination, 
					q -> q.ofProject(projectId).withType(AnnotationType.DEAD_CODE));
			
			LOG.debug(() -> String.format("Setting dead code for %d annotations for project %s", paged.getSize(), projectId));
			
			paged.getContent()
				.stream()
				.filter(annotation -> annotation.getOffset().isEmpty() || annotation.getLength().isEmpty())
				.forEach(annotation -> LOG.error(MISSING_ANNOTATION_LOCATION_MESSAGE, 
						annotation.identity(), 
						annotation.getLocation()));
			
			for (final AnnotationPojo annotation : paged.getContent().stream().filter(annotation -> 
																						annotation.getOffset().isPresent() && 
																						annotation.getLength().isPresent()).toList()) {
					moduleService.setFromDeadCode(q ->
						q.ofProject(projectId)
						.ofModuleInDirection(annotation.getModule(), RelationshipDirection.OUT)
						.withTypes(RELEVANT_RELATIONSHIP_TYPES)
						.withSourceLocationInRange(new ModuleLocation(
													annotation.getOffset().get(),
													annotation.getLength().get())),
						true);
			}
			
			size = paged.getSize();
			pagination = pagination.nextPage();
		} while (size == PAGE_LIMIT);
	}

	/** 
	 * Method to set the field fromDeadCode to true for reference from the given annotation where they are called originating in a deadCode
	 *
	 * @param projectId the project ID of the Annotation and Reference
	 * @param annotationId the ID of the Annotation
	 */
	public void setDeadCodeForAnnotation(final EntityId projectId, final EntityId annotationId) {
		final Optional<AnnotationPojo> optionalAnnotation = annotationService.findAny(q -> q.ofProject(projectId)
				.byId(annotationId)
				.withType(AnnotationType.DEAD_CODE));
		
		LOG.debug(() -> String.format("Setting dead code for annotation %s for project %s", annotationId, projectId));
		
		optionalAnnotation.ifPresent(annotation -> {
			
			if (annotation.getOffset().isPresent() && annotation.getLength().isPresent()) {
				moduleService.setFromDeadCode(q -> q.ofProject(projectId)
						.ofModuleInDirection(annotation.getModule(), RelationshipDirection.OUT)
						.withTypes(RELEVANT_RELATIONSHIP_TYPES)
						.withSourceLocationInRange(new ModuleLocation(
								annotation.getOffset().get(),
								annotation.getLength().get())),
						true);
			} else {
				LOG.error(MISSING_ANNOTATION_LOCATION_MESSAGE,
						annotation.identity(), 
						annotation.getLocation());
			}});
	}
	
	/**
	 * EventListener for {@link AnnotationCreatedEvent} to update the dead code flag for module relationships that are
	 * encompassed by dead code annotations.  If the annotation id is provided in the {@link AnnotationCreatedEvent}
	 * then the flag will only be set for that particular annotation, otherwise all annotations in the project
	 * will be evaluated.
	 * 
	 * @param event signaling the creation of an {@link AnnotationPojo}.
	 */
	@EventListener()
	public void onAnnotationCreated(final AnnotationCreatedEvent event) {
		event.getProjectId().ifPresent(projectId ->
			event.getAnnotationId().ifPresentOrElse(annotationId ->
					setDeadCodeForAnnotation(projectId, annotationId),
			() -> setDeadCodeForProject(projectId)));
	}
	
	/**
	 * EventListener for {@link AnnotationUpdatedEvent} to update the dead code flag for module relationships that are
	 * encompassed by dead code annotations. If the annotation id is provided in the {@link AnnotationUpdatedEvent}
	 * then only that annotation's module will be evaluated for dead code, otherwise all annotations in the project
	 * will be evaluated.
	 * 
	 * @param event signaling that an {@link AnnotationPojo} was updated.
	 */
	@EventListener()
	public void onAnnotationUpdated(final AnnotationUpdatedEvent event) {
		event.getProjectId().ifPresent(projectId ->
			event.getAnnotationId().ifPresentOrElse(annotationId -> {
				final Optional<AnnotationPojo> optionalAnnotation =
						annotationService.findAny(q -> q
										.ofProject(projectId)
										.byId(annotationId));
				
				optionalAnnotation.ifPresent(annotation -> setDeadCodeForModule(projectId, annotation.getModule()));
			},
			() -> setDeadCodeForProject(projectId)));
	}
	
	/**
	 * EventListener for {@link AnnotationDeletedEvent} to update dead code for module relationships that are
	 * encompassed by dead code annotations.  If the annotation id is provided in the {@link AnnotationDeletedEvent}
	 * then only that annotation's module will be evaluated for dead code, otherwise all annotations in the project
	 * will be evaluated. 
	 * 
	 * @param event signaling the deletion of an {@link AnnotationPojo}.
	 */
	@EventListener()
	public void onAnnotationDeleted(final AnnotationDeletedEvent event) {
		event.getProjectId().ifPresent(projectId ->
			event.getModuleId().ifPresentOrElse(moduleId ->
				setDeadCodeForModule(projectId, moduleId),
			() -> setDeadCodeForProject(projectId)));
	}
	
	/**
	 * EventListener for {@link AnnotationEvent} to update dead code.  Event instances of {@link AnnotationCreatedEvent},
	 * {@link AnnotationUpdatedEvent} ,and {@link AnnotationDeletedEvent} are no-ops for this listener as they
	 * are handled explicitly by other listeners.
	 * 
	 * @param event triggered on an {@link AnnotationEvent}.
	 */
	@EventListener
	public void onAnnotationEvent(final AnnotationEvent event) {
		if (event instanceof AnnotationCreatedEvent || event instanceof AnnotationUpdatedEvent || event instanceof AnnotationDeletedEvent) {
			return;
		}
		
		event.getProjectId().ifPresent(projectId ->
			event.getAnnotationId().ifPresentOrElse(annotationId -> {
				final Optional<AnnotationPojo> optionalAnnotation =
						annotationService.findAny(q -> q
										.ofProject(projectId)
										.byId(annotationId));
				
				optionalAnnotation.ifPresent(annotation -> setDeadCodeForModule(projectId, annotation.getModule()));
			},
			() -> setDeadCodeForProject(projectId)));
	}
		
	private boolean hasExistingDeadCodeAnnotation(final EntityId moduleId, final int lowestStartOffset, final int highestEndOffset) {
		return annotationService.findAny(q -> q.ofModule(moduleId)
										.withType(AnnotationType.DEAD_CODE)
										.withLocationInRange(new ModuleLocation(lowestStartOffset, highestEndOffset))).isPresent();
	}
	
	private void createDeadCodeAnnotation(final EntityId projectId, final EntityId moduleId, final int lowestStartOffset, final int highestEndOffset) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(DEAD_CODE_ANNOTATION_NAME);
		annotation.setModule(moduleId);
		annotation.setType(AnnotationType.DEAD_CODE);
		annotation.setState(WorkingState.CANDIDATE);
		annotation.setCreatedByUserId(SchemaConstants.SYSTEM_USER);
		annotation.setLocation(new ModuleLocation(lowestStartOffset, highestEndOffset - lowestStartOffset));
		final EntityId annotationId = annotationService.create(annotation);
		
		eventPublisher.publishEvent(new AnnotationCreatedEvent(projectId, annotationId));
	}
}