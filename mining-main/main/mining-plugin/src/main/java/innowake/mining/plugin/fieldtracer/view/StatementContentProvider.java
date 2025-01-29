/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.eclipse.jface.viewers.IStructuredContentProvider;

import innowake.ndt.fieldtracing.FieldTracerUtils;
import innowake.ndt.fieldtracing.model.FieldDefinition;
import innowake.ndt.fieldtracing.model.FieldUsage;
import innowake.ndt.fieldtracing.model.SourceFileOrFieldDefinition;
import innowake.ndt.fieldtracing.model.tree.TreeNode;

/**
 * Content provider for the statement table.
 * Input {@link FieldDefinition}
 * Output {@link FieldUsage} 
 */
final class StatementContentProvider implements IStructuredContentProvider {

	@Override
	public Object[] getElements(final Object inputElement) {
		if (inputElement.getClass().isArray()) {
			return FieldTracerUtils.filter(Arrays.asList((Object[]) inputElement), TreeNode.class).stream()
				.map(TreeNode::getData)
				.map(def -> (SourceFileOrFieldDefinition<?>) def)
				.filter(SourceFileOrFieldDefinition::isFieldDefinition)
				.flatMap(defNode -> defNode.getFieldDefinitionNotNull().getUsages().stream())
				.collect(Collectors.toList()).toArray();
		} else {
			throw new IllegalStateException("Array expected but was " + inputElement.getClass());
		}
	}

}
