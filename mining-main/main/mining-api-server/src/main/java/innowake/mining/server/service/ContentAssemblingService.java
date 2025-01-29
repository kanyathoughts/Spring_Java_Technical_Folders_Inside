package innowake.mining.server.service;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.DummyIAssembling;
import innowake.mining.data.core.MiningContentProvider;
import innowake.mining.data.core.OParseResult;
import innowake.mining.data.core.api.Model;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.codeviewer.AssembledContent;
import innowake.mining.shared.model.codeviewer.CodeViewerRange;
import innowake.mining.shared.model.codeviewer.Inclusion;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Inclusions;
import innowake.ndt.core.assembling.retrace.Part;
import innowake.ndt.core.assembling.retrace.Retracer;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.parsing.IDocument;
import innowake.ndt.core.parsing.ILocation;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.jcl.parser.api.JCLConfigParam;
import innowake.ndt.jcl.parser.api.JclLine;
import innowake.ndt.jcl.parser.assembling.JCLAssembler;
import innowake.ndt.jcl.parser.assembling.JCLBlockParser;
import innowake.ndt.jcl.parser.assembling.mvs.JCLMvsAssembler;
import innowake.ndt.jcl.parser.env.OSType;
import innowake.ndt.jcl.parser.model.JclContent;
import innowake.ndt.jcl.parser.parser.mvs.JCLMvsParser;

/**
 * Service providing assembled source code as well as information about the included source code for the code viewer.
 */
@Service
public class ContentAssemblingService {

    private static final Logger LOG = LoggerFactory.getLogger(ContentAssemblingService.class);

    @Autowired
    private MiningDataCoreService coreService;
    
    @Autowired
    private ModuleService moduleService;

    /**
     * Calculate the {@link AssembledContent} for a given module, which contains the assembled source code
     * as well as information about where the individual assembled parts come from.
     * @param projectId the project id
     * @param moduleId the module id
     * @return the assembled content for the module
     */
    public AssembledContent getAssembledContent(final EntityId projectId, final EntityId moduleId) {
        final var module = moduleService.findAnyModule(q -> q.ofProject(projectId).byId(moduleId).includeContent(true))
        		.orElseThrow(() -> new MiningEntityNotFoundException("Failed to load module with numeric id: " + moduleId + " in project: " + projectId));
        return getModuleAssembledContent(module);
    }

    /**
     * Calculate the {@link AssembledContent} for a given module, which contains the assembled source code
     * as well as information about where the individual assembled parts come from.
     * @param module the module
     * @return the assembled content for the module
     */
    public AssembledContent getModuleAssembledContent(final ModulePojo module) {

        if (module.getTechnology().equals(Technology.JCL)) {
            /* no standard assembling available for JCL - using a workaround */
            return getJclAssembledContent(module);
        }

        /* it is unnecessary to parse the module again, but it is the simplest way to get access to the Assembling
         * for all languages */
        final Optional<OParseResult> parseResult = coreService.getParseResult(module.identity());

        if ( ! parseResult.isPresent()) {
            LOG.error("While getting assembled content for code-viewer: Failed to parse Module " + module.getId() + ": unable to obtain parse result");
            return AssembledContent.unavailable();
        }
        final Optional<Model> model = parseResult.get().getModel();
        if ( ! model.isPresent()) {
            LOG.error("While getting assembled content for code-viewer: Failed to parse Module " + module.getId() + ": unable to obtain model from parse result");
            return AssembledContent.unavailable();
        }
        @SuppressWarnings("unchecked") /* we know the return type of Utils.getParseResult() */
        final IAssembling<ModulePojo> assembling = (IAssembling<ModulePojo>) model.get().getAssembling();
        if (assembling instanceof DummyIAssembling) {
            /* this language does not use assembling - unfortunately, we need to check it this way, currently */
            LOG.debug(() -> "Module " + module.getId() + " has no assembling information. Probably this type of module does not use assembling.");
            return AssembledContent.unavailable();
        }

        final String assembledContent = assembling.getAssembledContent();
        final IDocument document = new Document(assembledContent);
        final List<Inclusion> inclusions = getInclusions(assembling, document);
        return AssembledContent.of(assembledContent, inclusions);
    }

    private List<Inclusion> getInclusions(final IAssembling<ModulePojo> assembling, final IDocument document) {
        final List<Inclusion> codeViewerInclusions = new ArrayList<>();
        final Retracing<ModulePojo> retracing = new Retracer<ModulePojo>().retrace(assembling);
        final Inclusions<ModulePojo> inclusions = retracing.getInclusions();
        if (inclusions == null) {
            return Collections.emptyList();
        }
        for (final innowake.ndt.core.assembling.retrace.Inclusion<ModulePojo> inclusion : inclusions) {
            final Part partInRoot = inclusion.getParts().getPartInRoot();
            final ModulePojo callee = inclusion.getCallee();
            if (partInRoot == null || callee == null) {
                continue;
            }
            final ModuleLocation location = convertModuleLocation(partInRoot.getLocation());
            codeViewerInclusions.add(new Inclusion(
                    moduleService.findAnyModuleLightweight(q -> q.byId(callee.identity()))
                    					.orElseThrow(() -> new MiningEntityNotFoundException("Failed to load module with numeric id: " + callee.identity())),
                    location,
                    convertEditorRange(document, location)
            ));
        }
        return codeViewerInclusions;
    }

    private ModuleLocation convertModuleLocation(final ILocation location) {
        return new ModuleLocation(location.getOffset(), location.getLength());
    }

    @Nullable
    private CodeViewerRange convertEditorRange(final IDocument document, @Nullable final ModuleLocation moduleLocation) {
        if (moduleLocation == null) {
            return null;
        }
        final int startOffset = moduleLocation.getOffset();
        final int endOffset = moduleLocation.getOffset() + moduleLocation.getLength();
        /* converting to 1-based indexes here */
        return new CodeViewerRange(
                document.getLineNumber(startOffset) + 1,
                document.getColumnNumber(startOffset) + 1,
                document.getLineNumber(endOffset) + 1,
                document.getColumnNumber(endOffset) + 1);
    }

    private AssembledContent getJclAssembledContent(final ModulePojo module) {
        /* our JCL assembler / parser does not provide the standard IAssembling, so adding special handling here to get the assembled content
        * by "parsing" the special comments that are inserted by the JCL assembler */
        final JCLBlockParser.Type sourceType;
        switch (module.getType()) {
            case JOB:
                sourceType = JCLBlockParser.Type.Job;
                break;
            case PROC:
                sourceType = JCLBlockParser.Type.Proc;
                break;
            default:
                sourceType = JCLBlockParser.Type.Unknown;
        }
        final JCLMvsParser parser = new JCLMvsParser();
        final JCLConfigParam properties = new JCLConfigParam();
        final JCLMvsAssembler assembler = new JCLMvsAssembler(parser, true, properties);
        final MiningContentProvider contentProvider = new MiningContentProvider(coreService, module);
        final JclContent assembledContent = assembler.execute(JCLAssembler.split(StringUtils.trimToEmpty(module.getContent().orElse(null)), OSType.MVS),
                contentProvider, sourceType, module.getName());

        /* find PROCs in the assembled content */
        final List<Inclusion> inclusions = new ArrayList<>();
        final Deque<String> currentProc = new ArrayDeque<>();
        final Deque<Integer> startLine = new ArrayDeque<>();
        final Deque<Integer> startOffset = new ArrayDeque<>();
        int currentOffset = 0;
        int currentLine = 1;
        for (final JclLine line : assembledContent.getLines()) {
            final String[] split = StringUtils.split(line.getText(), ' ');

            if (split.length == 7) {
                /* check if line matches "//* <$ BEGIN OF PROC APROC $>" */
                if (split[2].equals("BEGIN") && split[3].equals("OF") && split[4].equals("PROC")) {
                    currentProc.push(split[5]);
                    startLine.push(currentLine);
                    startOffset.push(currentOffset);
                }

                /* check if line matches "//* <$ END OF PROC APROC $>" */
                if ( ! currentProc.isEmpty() && split[2].equals("END") && split[3].equals("OF") && split[4].equals("PROC")) {
                    final String procName = currentProc.pop();
                    final int currentStartOffset = startOffset.pop();
                    final int currentStartLine = startLine.pop();
                    final Optional<ModuleLightweightPojo> referencedModule = findReferencedProc(module, contentProvider, procName);
                    if (referencedModule.isPresent()) {
                        inclusions.add(new Inclusion(referencedModule.get(),
		                                new ModuleLocation(currentStartOffset, currentOffset - currentStartOffset),
		                                new CodeViewerRange(currentStartLine, 0, currentLine, 0)));
                    }
                }
            }

            currentLine++;
            currentOffset += line.getText().length();
        }

        return AssembledContent.of(assembledContent.getContent(), inclusions);
    }

    private Optional<ModuleLightweightPojo> findReferencedProc(final ModulePojo moduleDescription, final MiningContentProvider contentProvider, final String name) {
        /* JCL uses CALLS for PROCs, not INCLUDES */
        final ModulePojo procModuleDescription = contentProvider.createModuleDescription(moduleDescription, name, RelationshipType.CALLS, Type.PROC);
        return Optional.ofNullable(procModuleDescription).map(moduleDesc -> moduleService.findAnyModuleLightweight(q -> q.byId(moduleDesc.identity()))
        					.orElseThrow(() -> new MiningEntityNotFoundException("Failed to load module with numeric id: " + moduleDesc.identity())));
    }
}
