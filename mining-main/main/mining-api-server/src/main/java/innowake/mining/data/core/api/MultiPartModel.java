/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.List;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Special implementation of {@link Model} that an hold multiple parts of a parser model.
 */
public class MultiPartModel implements Model {
	
	/** The actual {@link ModelPart}s. */
	public final List<ModelPart> modelParts;

	/** The full parse model. */
	public final AstModel combinedModel;
	
	/** The whole source content of all assembling parts. */
	@Nullable
	public final String combinedAssembledContent;
	
	/**
	 * Constructor.
	 * 
	 * @param parseModels the parse models
	 * @param combinedModel the full model comprised of all model parts
	 * @param combinedAssembledContent the full assembled content based on {@code combinedModel}
	 */
	public MultiPartModel(final List<ModelPart> parseModels, final AstModel combinedModel, @Nullable final String combinedAssembledContent) {
		this.modelParts = parseModels;
		this.combinedModel = combinedModel;
		this.combinedAssembledContent = combinedAssembledContent;
	}
	
	@Override
	public AstModel getParseModel() {
		return combinedModel;
	}
	
	@Override
	public Object getAssembling() {
		return modelParts.stream().map(modelPart -> modelPart.assembling).collect(Collectors.toList());
	}
	
	@Override
	@Nullable
	public String getAssembledContent() {
		return combinedAssembledContent;
	}
	
	/**
	 * Contains a partial parse model, its assembling and a {@link TargetPartPosition} when combining the separate parts.
	 */
	public static class ModelPart {
		
		/**
		 * The position a part should receive when combining the separate parts.
		 */
		public enum TargetPartPosition {
			ROOT,
			FIRST_CHILD,
			LAST_CHILD;
		}
		
		public final AstModel model;
		public final IAssembling<ModulePojo> assembling;
		public final TargetPartPosition partPosition;
		
		/**
		 * Constructor.
		 * 
		 * @param model the {@link AstModel}
		 * @param assembling the {@link IAssembling} for the {@code model}
		 * @param partPosition the {@link TargetPartPosition}
		 */
		public ModelPart(final AstModel model, final IAssembling<ModulePojo> assembling, final TargetPartPosition partPosition) {
			this.model = model;
			this.assembling = assembling;
			this.partPosition = partPosition;
		}
		
	}
}
