/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import org.eclipse.jface.resource.ImageDescriptor;

import innowake.base.eclipse.common.ui.util.ImageDescriptorFactory;
import innowake.lib.core.lang.Assert;
import innowake.mining.plugin.MiningPlugin;

/**
 * Class to maintain the links to image sources for BASIC editor and outline page.
 */
public class BasicImages {
	
	private BasicImages() {
		/* Preventing Instantiation */
	}
	
	private static final ImageDescriptorFactory F = new ImageDescriptorFactory(Assert.assertNotNull(MiningPlugin.getDefault()).getBundle(), "icons/");
	
	/**
	 * Descriptor outline BASIC node.
	 */
	public static final ImageDescriptor OUTLINE_NODE_BASIC = F.create("basicNode.gif"); 

}
