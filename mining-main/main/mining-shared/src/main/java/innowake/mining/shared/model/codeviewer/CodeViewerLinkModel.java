/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

/**
 * Model containing hyperlinks for the code viewer, allowing navigation from one location in the code to another location,
 * either in the same file or a different file
 */
public class CodeViewerLinkModel {

    private final List<CodeViewerLink> links;

    @JsonCreator
    public CodeViewerLinkModel(@JsonProperty("links") final List<CodeViewerLink> links) {
        this.links = links;
    }

    /**
     * Get the list of links in the model.
     * @return the list of links
     */
    public List<CodeViewerLink> getLinks() {
        return links;
    }

	@Override
	public int hashCode() {
		return Objects.hash(links);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final CodeViewerLinkModel other = (CodeViewerLinkModel) obj;
		return Objects.equals(links, other.links);
	}
    
    
}
