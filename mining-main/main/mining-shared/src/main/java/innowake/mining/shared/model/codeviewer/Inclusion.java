/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Contains information about content included from other files in an {@link AssembledContent}.
 */
public class Inclusion {

    private final ModuleLightweightPojo originModule;
    private final ModuleLocation assembledLocation;
    private final CodeViewerRange assembledRange;

    @JsonCreator
    public Inclusion(@JsonProperty("originModule") final ModuleLightweightPojo originModule,
                     @JsonProperty("assembledLocation") final ModuleLocation assembledLocation,
                     @JsonProperty("assembledRange") final CodeViewerRange assembledRange) {
        this.originModule = originModule;
        this.assembledLocation = assembledLocation;
        this.assembledRange = assembledRange;
    }

    /**
     * Returns the Module from where this source code was included.
     * @return the origin Module of the included source code
     */
    public ModuleLightweightPojo getOriginModule() {
        return originModule;
    }

    /**
     * Returns the location in the final, assembled document where this included content is located.
     * @return the location of the included content in the assembled document
     */
    public ModuleLocation getAssembledLocation() {
        return assembledLocation;
    }

    /**
     * Returns the location in the final, assembled document where this included content is located,
     * expressed in line numbers and columns.
     * @return the location of the included content in the assembled document
     */
    public CodeViewerRange getAssembledRange() {
        return assembledRange;
    }
}
