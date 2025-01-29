/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.jcl;

import com.google.common.collect.Sets;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dna.sequencer.Sequencer;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.ndt.jcl.parser.model.ast.JclAstNode;
import innowake.ndt.jcl.parser.model.ast.JclJobNode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Sequencers for JCL. <br>
 * <ul>
 *     <li>Method Rule: Forms DNA string by extracting the target module name for each step.</li>
 *     <li>Skeleton Rule: Forms DNA string by extracting the target module type for each step.</li>
 * </ul>
 *
 * Example:
 * <pre>
 * //JCL1          JOB %ACCTGK,'%OADID',CLASS=S,MSGCLASS=Q,REGION=7M
 * //STEP1         EXEC PGM=IKJEFT01,DYNAMNBR=20
 * //STEPLIB       DD   DSN=ENDEVOR.PRODP.LOADBAT,DISP=SHR
 * //              DD   DSN=SYS1.GDB2FACH.LINKLIB,DISP=SHR
 * //SYSTSPRT      DD   SYSOUT=*
 * //* IKJEFT01 : COBOLCall in PROG($NAME)
 * //SYSTSIN       DD *
 *  DSN SYSTEM(DB2F)
 *  RUN PROG(G2BPES01)
 *      PLAN(GKPLANB)
 *      PARMS('$GK103')
 *  END
 * /*
 * //STEP2         EXEC PGM=IEFBR14,DYNAMNBR=20
 * //STEP3         EXEC PGM=CBLPGM,DYNAMNBR=20
 * //STEP4         EXEC PROC1
 * //PROC1 	   PROC
 * //STEP5         EXEC PGM=IEFBR14,DYNAMNBR=20
 * //PROC1 	   PEND
 * </pre>
 * <p> In the above JCL there are 5 steps and hence there will be at least 5 string elements for each sequencer rules. </p>
 * <p> For Method Rule, the target module name for each step is extracted and if the target module is a PROC then all the steps inside the PROC is expanded
 * and it's target module name is used to form the DNA string. For certain steps such as "STEP1" in the above case have two target modules, in such cases both
 * the target modules are used to form the DNA string. The order within the step targets are determined alphabetically. </p>
 * <p> Following is the possible DNA string: G2BPES01, IKJEFT01, IEFBR14, CBLPGM, PROC1, IEFBR14 </p>
 * <p> For Skeleton Rule, same as Method Rule, but here the target module type for each step is considered. If the target module is a Utility, then instead of
 * the type, the utility name is used to form the DNA string. </p>
 * <p> Following is the possible DNA string: PROGRAM, IKJEFT01, IEFBR14, PROGRAM, PROC, IEFBR14 </p>
 */
@Component
public class JclSequencer implements Sequencer {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);
	@Autowired
	private CallChainService callChainService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ExecutorService executorService;
	@Autowired
	private AstService astService;

	@Override
	public List<Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>>> apply(final SourcePojo sourceObject) {
		LOG.debug("Applying JCL sequencer to {}", sourceObject.getPath());
		return Arrays.asList(applyMethodRule(sourceObject), applySkeletonRule(sourceObject));
	}

	private Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>> applyMethodRule(final SourcePojo sourceObject) {
		return applyInternal(sourceObject, ModuleLightweightPojo::getName, DnaSequencer.JCL_METHOD_RULE);
	}

	private Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>> applySkeletonRule(final SourcePojo sourceObject) {
		return applyInternal(sourceObject, module -> module.getType() == Type.UTILITY
				? module.getName()
				: module.getType()
						.name(), DnaSequencer.JCL_SKELETON_RULE);
	}

	private Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>> applyInternal(final SourcePojo sourceObject,
			final Function<ModuleLightweightPojo, String> dnaStringExtractor, final DnaSequencer sequencerRule) {
		final List<EntityId> moduleIds = moduleService.findModuleIds(q -> q.ofProject(sourceObject.getProject())
																			.withPath(sourceObject.getPath())
																			.withTechnology(sourceObject.getTechnology())
																			.withType(sourceObject.getType())
																			.limit(1));
		if (moduleIds.isEmpty()) {
			throw new MiningEntityNotFoundException(String.format("Module in project: %s, with path: %s, technology: %s and type: %s must exists ", 
					sourceObject.getProject(), sourceObject.getPath(), sourceObject.getTechnology(), sourceObject.getType()));
		}

		/* Fetch all job steps' module in order */
		final List<ModuleLightweightPojo> stepModulesInOrder = fetchJobStepsInOrder(sourceObject.getProject(), moduleIds.get(0));
		final Parameters parameters = new Parameters.Builder()
				.setProjectId(sourceObject.getProject())
				.setDirections(Arrays.asList(CallChain.CallChainDirection.OUT))
				.setCallTypes(Sets.newHashSet(RelationshipType.CALLS, RelationshipType.REFERENCES))
				.setStartModuleIds(Collections.singletonList(moduleIds.get(0)))
				.setParallel(1)
				.setEndModuleTypes(Sets.newHashSet(Type.PROGRAM, Type.UTILITY, Type.UNKNOWN))
				.build();

		/* Using call chain, determine the target module that each step invokes */
		final Optional<List<CallChainGraph>> callChains = callChainService.createCallChainGraphs(new NullProgressMonitor(), parameters);

		final List<DnaStringElementPojoPrototype> dnaStringElements = new ArrayList<>();
		final Map<Long, List<String>> mapOfStepIdToDnaStrings = new HashMap<>();
		final Consumer<CallChain> callChainConsumer = callChain -> {
			final List<String> dnaStrings = new ArrayList<>();
			boolean flag = false;
			Long stepModuleId = null;
			for (final CallChain.CallChainEntry callChainEntry : callChain.getCallChainEntries()) {
				final ModuleLightweightPojo module = callChainEntry.getModule();
				if (module.getType() == Type.EXEC || module.getType() == Type.EXEC_PGM) {
					flag = true;
					stepModuleId = module.getId();
					continue;
				}
				if (flag) {
					dnaStrings.add(dnaStringExtractor.apply(module));
				}
			}
			if (flag) {
				mapOfStepIdToDnaStrings.computeIfPresent(stepModuleId, (k, v) -> {
					v.addAll(dnaStrings);
					return v;
				});
				mapOfStepIdToDnaStrings.putIfAbsent(stepModuleId, dnaStrings);
			}
		};

		if (callChains.isPresent() && ! callChains.get().isEmpty()) {
			callChainService.traverseGraph(callChains.get().get(0), parameters, callChainConsumer, new HashSet<>());
		}

		/* Now using the ordered step modules that was determined earlier and the dna string elements computed using the callchain,
		determine the index and build the complete dna string elements   */
		final AtomicInteger index = new AtomicInteger(0);
		stepModulesInOrder.stream()
				.map(ModuleLightweightPojo::getId)
				.forEach(stepId -> {
					final List<String> strings = mapOfStepIdToDnaStrings.get(stepId);
					if (strings != null) {
						strings.stream()
								.sorted()
								.forEach(s -> dnaStringElements.add(new DnaStringElementPojoPrototype().setIndex(index.getAndIncrement())
										.setValue(s)
										.setLocation(new ModuleLocation(0, 0))));
					}
				});
		return Tuple2.of(new DnaStringPojoPrototype()
								.setSequencer(sequencerRule)
								.setContentHash(sourceObject.getContentHash().get()), dnaStringElements);

	}

	/**
	 * Fetches all job steps in order from the StoreAst.
	 * Firstly, it stores the AST for the module with the provided Id. Then it fetches the children of JclJobNode from the AST which is the root node of a JCL.
	 * The downside is, it cannot handle nested JCL statement such as Steps inside IF-ELSE block.
	 *
	 * @param projectEid the project id
	 * @param moduleEid the module id of the JOB
	 * @return the list of job steps in order
	 */
	private List<ModuleLightweightPojo> fetchJobStepsInOrder(final EntityId projectEid, final EntityId moduleEid) {
		LOG.debug("Performing store AST for module id: {}", moduleEid);
		executorService.executeStoreAst(projectEid, moduleEid);
		LOG.debug("Store AST for module id: {} completed", moduleEid);

		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleEid).withType(JclJobNode.class.getSimpleName()));
		if (astNodes.isEmpty()) {
			return Collections.emptyList();
		} else if (astNodes.size() > 1) {
			LOG.warn("Found more than one JclJobNode for module id: {}", moduleEid);
			return Collections.emptyList();
		}

		/* Fetch all job steps from the JclJobNode */
		final List<String> jobStepNames = astService.find(q -> q.ofParent(astNodes.get(0).getId())).stream()
				.map(node -> (String) node.getProperties().get(JclAstNode.QUALIFIED_STEP_NAME))
				.filter(Objects::nonNull)
				.collect(Collectors.toList());

		final Map<String, ModuleLightweightPojo> mapOfJobStepNameToModuleLightweight = moduleService
			.findModulesLightweight(q -> q.ofProject(projectEid).withNames(jobStepNames).withTypes(Arrays.asList(Type.EXEC, Type.EXEC_PGM))).stream()
				.collect(Collectors.toMap(ModuleLightweightPojo::getName, moduleLightweight -> moduleLightweight));

		return jobStepNames.stream()
				.map(mapOfJobStepNameToModuleLightweight::get)
				.filter(Objects::nonNull)
				.collect(Collectors.toList());
	}

}
