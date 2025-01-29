/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Collection;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.jface.text.ITextSelection;

import innowake.lib.core.api.lang.Nullable;

/**
 * Tests if the current selection is an {@link ITextSelection}.
 */
public class IsTextSelectionPropertyTester extends PropertyTester {

	@Override
	public boolean test(
			@Nullable Object receiver, 
			@Nullable String property, 
			@Nullable Object[] args, 
			@Nullable Object expectedValue) {
		
		if ("isTextSelection".equalsIgnoreCase(property) && receiver instanceof Collection<?>) {
			final Collection<?> receiverCollection = (Collection<?>) receiver;
			if (receiverCollection.size() == 1 && receiverCollection.iterator().next() instanceof ITextSelection) {
				return true;
			} 
		}
		
		return false;
	}
}
