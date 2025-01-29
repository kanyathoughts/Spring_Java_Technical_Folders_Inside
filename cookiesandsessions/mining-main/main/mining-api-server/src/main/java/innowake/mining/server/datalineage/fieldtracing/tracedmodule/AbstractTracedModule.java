/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.tracedmodule;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Optional;

import com.google.common.collect.Streams;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.ndt.fieldtracing.model.TracedModule;

/**
 * Implementation of the TracedModule provided by the fieldtracing library.
 * This class gets extended by the type specific modules.
 */
abstract class AbstractTracedModule implements TracedModule<ModuleLightweightPojo> {

	protected final DataLineageContext context;
	protected final ModuleLightweightPojo module;
	protected final SourcePojo sourceObject;
	
	protected final TracedModuleProvider tracedModuleProvider;
	
	protected AbstractTracedModule(final DataLineageContext context, final ModuleLightweightPojo module, final SourcePojo sourceObject,
			final TracedModuleProvider tracedModuleProvider) {
		this.context = context;
		this.module = module;
		this.sourceObject = sourceObject;
		this.tracedModuleProvider = tracedModuleProvider;
	}

	@Override
	public String getName() {
		return module.getName();
	}

	@Override
	public String getContent() {
		return sourceObject.getContent().toString();
	}

	@Override
	public Path getPath() {
		return Paths.get(module.getPath());
	}

	@Override
	public Type getType() {
		return miningTypeToFieldTracingType(module.getType());
	}

	@Override
	public Language getLanguage() {
		final Technology technology = module.getTechnology();
		switch(technology) {
		case COBOL:
			return Language.COBOL;
		case NATURAL:
			return Language.NATURAL;
		default:
			throw new IllegalArgumentException("Tracing a field for " + technology.toString() + " is not supported.");
		}
	}

	@Override
	public Optional<TracedModule<ModuleLightweightPojo>> findObject(final String name, final Type type) {
		final Technology technology = this.module.getTechnology();
		final innowake.mining.shared.model.Type miningType = fieldTracingTypeToMiningType(type);

		return Streams.concat(getOutgoingDeps().stream(), getIncomingDeps().stream())
				.filter(tracedModule -> tracedModule.getSourceObject().getName().equalsIgnoreCase(name)
						&& tracedModule.getSourceObject().getTechnology().equals(technology)
						&& tracedModule.getSourceObject().getType().equals(miningType))
				.findFirst();
	}

	/**
	 * Converts from a fieldtracing Type to a mining Type
	 * @param type The fieldtracing Type
	 * @return The converted mining Type
	 */
	private innowake.mining.shared.model.Type fieldTracingTypeToMiningType(final Type type) {
		switch(type) {
		case COPYBOOK:
			return innowake.mining.shared.model.Type.COPYBOOK;
		case PROGRAM:
			return innowake.mining.shared.model.Type.PROGRAM;
		case BMS:
			return innowake.mining.shared.model.Type.BMS_MAP;
		default:
			throw new IllegalArgumentException("Type " + type + " can not be converted to a mining type");
		}	
	}
	
	/**
	 * Converts from a mining Type to a fieldtracing Type
	 * @param type The mining Type
	 * @return The converted fieldtracing Type
	 */
	private Type miningTypeToFieldTracingType(final innowake.mining.shared.model.Type type) {
		switch(type) {
		case COPYBOOK:
			return Type.COPYBOOK;
		case PROGRAM:
			return Type.PROGRAM;
		case BMS_MAP:
			return Type.BMS;
		default: 
			throw new IllegalArgumentException("Type " + type + " can not be converted to a fieldtracing type");
		}
	}
	
	@Override
	public ModuleLightweightPojo getSourceObject() {
		return module;
	}
	
	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof AbstractTracedModule)) {
			return false;
		}
		final AbstractTracedModule that = (AbstractTracedModule) o;
		return Objects.equals(context.getProjectId(), that.context.getProjectId()) && Objects.equals(module.getId(), that.module.getId());
	}

	@Override
	public int hashCode() {
		return Objects.hash(context.getProjectId(), module.getId());
	}

	
	public ModuleLightweightPojo getModule() {
		return module;
	}
	
	public Long getModuleId() {
		return this.module.getId();
	}

}

