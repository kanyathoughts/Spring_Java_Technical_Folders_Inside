/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.hyperlink;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.hyperlink.IHyperlink;

import innowake.lib.core.api.lang.Nullable;

/**
 * An abstract base class for all hyperlink (s).
 */
public abstract class IdeHyperlink implements IHyperlink  {
	
	private final IRegion region;
	private final IRegion targetRegion;
	
	/**
	 * Constructor to initialize {@link IdeHyperlink}.
	 * 
	 * @param region region of the hyperlink
	 * @param targetRegion of the hyperlink
	 */
	public IdeHyperlink(final IRegion region, final IRegion targetRegion) {
		this.region = region;
		this.targetRegion = targetRegion;
	}
	
	@Override
	public IRegion getHyperlinkRegion() {
		return region;
	}
	
	@Override
	@Nullable
	public String getTypeLabel() {
		return null;
	}
	
	@Override
	@Nullable
	public String getHyperlinkText() {
		return null;
	}
	
	/**
	 * Returns the target region of the hyperlink.
	 *
	 * @return target region of the hyperlink
	 */
	protected IRegion getTargetRegion() {
		return targetRegion;
	}

}
