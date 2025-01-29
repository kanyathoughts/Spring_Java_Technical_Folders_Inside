/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.data.plugin.additionalinfo;

import java.util.Optional;

import com.orientechnologies.orient.core.hook.ORecordHookAbstract;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.ORecord;
import com.orientechnologies.orient.core.record.OVertex;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.plugin.Logging;

/**
 * Trigger that runs whenever a HasAdditionalInfo edge is deleted. It deletes the AdditionalInfo vertex that edge pointed to, if no other edge points to it.
 */
public class DeleteOrphanedAdditionalInfoHook extends ORecordHookAbstract {

	/* this hook targets the HasAdditionalInfo edge */
	public static final String TARGET_CLASS_NAME = "HasAdditionalInfo";
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_DATA_PLUGIN);

	@Override
	public RESULT onRecordBeforeDelete(final @Nullable ORecord iRecord) {
		LOG.debug(() -> ("Hook running on " + iRecord));
		if (iRecord instanceof OElement) {
			final OElement iElement = (OElement) iRecord;
			final Optional<OClass> maybeClass = iElement.getSchemaType();
			final Optional<OEdge> maybeEdge = iElement.asEdge();
			if (maybeClass.isPresent() && TARGET_CLASS_NAME.equals(maybeClass.get().getName()) && maybeEdge.isPresent()) {
				final OVertex targetVertex = maybeEdge.get().getTo();
				if (targetVertex != null && ! targetVertex.getEdges(ODirection.IN, maybeClass.get()).iterator().hasNext()) {
					LOG.debug(() -> ("Hook deleted vertex " + targetVertex.getIdentity()));
					targetVertex.delete();
				}
			}
		}

		return RESULT.RECORD_NOT_CHANGED;
	}

	@Override
	public DISTRIBUTED_EXECUTION_MODE getDistributedExecutionMode() {
		return DISTRIBUTED_EXECUTION_MODE.BOTH;
	}
}
