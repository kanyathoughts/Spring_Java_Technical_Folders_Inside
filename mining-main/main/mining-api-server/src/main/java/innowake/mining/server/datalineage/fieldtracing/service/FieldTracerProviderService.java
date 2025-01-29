/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.service;

import innowake.mining.server.datalineage.context.DataLineageContext;
import org.springframework.stereotype.Service;

import innowake.lib.core.IProgress;
import innowake.lib.job.api.ProgressMonitorAdapter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.Technology;
import innowake.ndt.fieldtracing.cobol.relatedfields.CobolDefaultRelatedFieldsResolver;
import innowake.ndt.fieldtracing.cobol.tracer.CobolFieldTracer;
import innowake.ndt.fieldtracing.legacy.tracer.IFieldTracer;
import innowake.ndt.fieldtracing.natural.relatedfields.NaturalDefaultRelatedFieldsResolver;
import innowake.ndt.fieldtracing.natural.tracer.NaturalFieldTracer;

/**
 * This class provides the correct FieldTracer provided by ndt-fieldtracing depending on which technology the tracing was invoked on.
 */
@Service
public class FieldTracerProviderService {

	/**
	 * Provides a FieldTracer
	 * @param context the context of the data lineage operation in which the field tracer is used
	 * @param technology The technology the tracing was invoked on
	 * @return Returns the correct FieldTracer
	 * @throws IllegalArgumentException Gets thrown when a technology is passed in which is not supported
	 */
	public IFieldTracer<ModuleLightweightPojo> provideFieldTracer(final DataLineageContext context, final Technology technology) throws IllegalArgumentException {
		/* Adapter between a ProgressMonitor and a IProgress*/ 
		final IProgress iMonitor = new ProgressMonitorAdapter(context.getProgressMonitor());
		
		switch (technology) {
		case COBOL:
			return new CobolFieldTracer<>(iMonitor, new CobolDefaultRelatedFieldsResolver<>(), context.getCachingFunctionForFieldTracer());
		case NATURAL:
			return new NaturalFieldTracer<>(new NaturalDefaultRelatedFieldsResolver<>(), iMonitor);
		default:
			throw new IllegalArgumentException("Tracing a field for " + technology.toString() + " is not supported.");
		}
	}
}
