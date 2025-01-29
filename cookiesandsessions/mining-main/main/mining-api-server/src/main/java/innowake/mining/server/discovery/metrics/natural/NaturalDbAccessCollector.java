/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiValuedMap;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.collection.MultiValueMapUtil;
import innowake.lib.parsing.util.visitor.TopDown;
import innowake.lib.parsing.util.visitor.Visitable;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.LocationHelper;
import innowake.ndt.naturalparser.ast.DataDefinition.View;
import innowake.ndt.naturalparser.ast.NaturalNode;
import innowake.ndt.naturalparser.ast.NaturalSwitch;
import innowake.ndt.naturalparser.ast.Statement;
import innowake.ndt.naturalparser.ast.Statement.DefineSubroutineStmt;
import innowake.ndt.naturalparser.ast.Statement.DeleteStmt;
import innowake.ndt.naturalparser.ast.Statement.FindStmt;
import innowake.ndt.naturalparser.ast.Statement.GetStmt;
import innowake.ndt.naturalparser.ast.Statement.HistogramStmt;
import innowake.ndt.naturalparser.ast.Statement.ReadStmt;
import innowake.ndt.naturalparser.ast.Statement.ReadWithoutDescriptorStmt;
import innowake.ndt.naturalparser.ast.Statement.StoreStmt;
import innowake.ndt.naturalparser.ast.Statement.UpdateStmt;

/**
 * Collects all database operations and the DDMs being used for them.
 */
public class NaturalDbAccessCollector extends NaturalSwitch implements Visitor {
	
	/**
	 * Natural database access types.
	 */
	public enum AccessType {
		READ,
		FIND,
		HISTOGRAM,
		GET,
		STORE,
		UPDATE,
		DELETE;
	}
	
	private final SourcePojo sourceObject;
	private final NaturalParseResultProvider naturalParserResultProvider;
	
	private MultiValuedMap<Class<?>, Statement> nodeCache = MultiValueMapUtil.createArrayListHashMap();
	private Map<String, Statement> labelToStatement = new HashMap<>();
	private final MultiValuedMap<String, AccessType> ddmAccess = MultiValueMapUtil.createArrayListHashMap();
	private Map<String, String> viewToDdm = new HashMap<>();
	
	public NaturalDbAccessCollector(final SourcePojo sourceObject, final NaturalParseResultProvider naturalParserResultProvider) {
		this.sourceObject = sourceObject;
		this.naturalParserResultProvider = naturalParserResultProvider;
	}
	
	/**
	 * Starts collecting the database access information.
	 * 
	 * @return mapping from DDM name to {@link AccessType}
	 * @throws DiscoveryException if the Natural module cannot be parsed
	 */
	public MultiValuedMap<String, AccessType> doCollect() throws DiscoveryException {
		final INaturalModel model = naturalParserResultProvider.getParseResult(sourceObject).getHeavyweightModel();
		new TopDown(this).visit(model.getProgramObject());
		return ddmAccess;
	}
	
	@Override
	public boolean visit(@Nullable final Object node) {
		if (node instanceof NaturalNode) {
			handle(node);
		}
		return true;
	}
	
	@Override
	protected void handleView(@Nullable final View node) {
		final View view = assertNotNull(node);
		viewToDdm.put(view.getName().toString(), view.getDdmName().toString());
	}
	
	@Override
	protected void handleReadWithoutDescriptorStmt(@Nullable final ReadWithoutDescriptorStmt node) {
		handleStatement(assertNotNull(node), AccessType.READ);
	}
	
	@Override
	protected void handleReadStmt(@Nullable final ReadStmt node) {
		handleStatement(assertNotNull(node), AccessType.READ);
	}
	
	@Override
	protected void handleGetStmt(@Nullable final GetStmt node) {
		handleStatement(assertNotNull(node), AccessType.GET);
	}
	
	@Override
	protected void handleFindStmt(@Nullable final FindStmt node) {
		handleStatement(assertNotNull(node), AccessType.FIND);
	}
	
	@Override
	protected void handleHistogramStmt(@Nullable final HistogramStmt node) {
		handleStatement(assertNotNull(node), AccessType.HISTOGRAM);
	}
	
	@Override
	protected void handleStoreStmt(@Nullable final StoreStmt node) {
		handleStatement(assertNotNull(node), AccessType.STORE);
	}
	
	@Override
	protected void handleDeleteStmt(@Nullable final DeleteStmt node) {
		final DeleteStmt stmt = assertNotNull(node);
		final CharSequence label = stmt.getLabel();
		handleDeleteOrUpdateStmt(stmt, label != null ? label.toString() : null, AccessType.DELETE);
	}
	
	@Override
	protected void handleUpdateStmt(@Nullable final UpdateStmt node) {
		final UpdateStmt stmt = assertNotNull(node);
		final CharSequence label = stmt.getLabel();
		handleDeleteOrUpdateStmt(stmt, label != null ? label.toString() : null, AccessType.UPDATE);
	}
	
	private void handleDeleteOrUpdateStmt(final Statement node, @Nullable final String label, final AccessType accessType) {
		Statement refStmt = null;
		/* DELETE/UPDATE can either reference an explicit previous label or the parent loop statement */
		if (label != null) {
			refStmt = labelToStatement.get(label);
		} else {
			refStmt = findParentDbStatement(node);
		}
		
		if (refStmt != null) {
			handleStatement(refStmt, accessType);
		}
	}
	
	private void handleStatement(final Statement statement, final AccessType accessType) {
		mapLabel(statement);
		nodeCache.put(statement.getClass(), statement);
		final String ddmName = viewToDdm.get(getViewName(statement));
		if (ddmName != null && ! ddmAccess.containsMapping(ddmName, accessType)) {
			ddmAccess.put(ddmName, accessType);
		}
	}
	
	private void mapLabel(final Statement node) {
		/* map from label to statement */
		final Object parent = node.getParent();
		if (parent instanceof Statement.Labeled) {
			final String labelName = ((Statement.Labeled) parent).getLabel().toString();
			labelToStatement.put(labelName, node);
		}
	}
	
	private String getViewName(final Statement node) {
		if (node instanceof FindStmt) {
			return ((FindStmt) node).getName().toString();
		} else if (node instanceof ReadStmt) {
			return ((ReadStmt) node).getName().toString();
		} else if (node instanceof ReadWithoutDescriptorStmt) {
			return ((ReadWithoutDescriptorStmt) node).getName().toString();
		} else if (node instanceof HistogramStmt) {
			return ((HistogramStmt) node).getName().toString();
		} else if (node instanceof GetStmt) {
			return ((GetStmt) node).getName().toString();
		} else if (node instanceof StoreStmt) {
			return ((StoreStmt) node).getName().toString();
		} else {
			throw new IllegalStateException("Unsupported statement type: " + node.getClass().getCanonicalName());
		}
	}
	
	@Nullable
	protected Statement findParentDbStatement(final NaturalNode node) {
		Statement result = null;
		Visitable parent = node.getParent();
		
		/* first check parent loops */
		while (result == null && parent != null) {
			if (parent instanceof DefineSubroutineStmt) {
				break;
			} else {
				if (parent instanceof FindStmt) {
					result = (FindStmt) parent;
				} else if (parent instanceof ReadStmt) {
					result = (ReadStmt) parent;
				} else if (parent instanceof ReadWithoutDescriptorStmt) {
					result = (ReadWithoutDescriptorStmt) parent;
				} else if (parent instanceof HistogramStmt) {
					result = (HistogramStmt) parent;
				} else {
					parent = parent.getParent();
				}
			}
		}
		
		/* if there's no parent database loop, check GET or FIND NUMBER on same level. */
		if (result == null) {
			int nodeLeft = LocationHelper.getLeft(node);
			if (LocationHelper.isValid(nodeLeft)) {
				final GetStmt lastGet = getLastGetStmt(node);
				if (lastGet != null && node != lastGet && nodeLeft >= lastGet.getLeft()) {
					result = lastGet;
				}
				if (result == null) {
					final FindStmt lastFindNumber = getLastFindNumberStmt(node);
					if (lastFindNumber != null && node != lastFindNumber && nodeLeft >= lastFindNumber.getLeft()) {
						result = lastFindNumber;
					}
				}
			}
		} 
		
		return result;
	}
	
	@Nullable
	protected GetStmt getLastGetStmt(final NaturalNode node) {
		final Collection<Statement> getStatments = nodeCache.get(GetStmt.class);
		if (CollectionUtils.isEmpty(getStatments)) {
			return null;
		}

		GetStmt precedingGet = null;
		int nodeLeft = LocationHelper.getLeft(node);
		if (LocationHelper.isValid(nodeLeft)) {
			for (final Statement get : getStatments) {
				if (get.getLeft() < nodeLeft) {
					precedingGet = (GetStmt) get;
				} else {
					break;
				}
			}
		}
		return precedingGet;
	}
	
	@Nullable
	protected FindStmt getLastFindNumberStmt(final NaturalNode node) {
		final Collection<Statement> findStatements = nodeCache.get(FindStmt.class);
		if (CollectionUtils.isEmpty(findStatements)) {
			return null;
		}

		FindStmt precedingFind = null;
		int nodeLeft = LocationHelper.getLeft(node);
		if (LocationHelper.isValid(nodeLeft)) {
			for (Statement find : findStatements) {
				if (((FindStmt) find).getNumberOfRecords().isNumberRecords()) {
					if (find.getLeft() < nodeLeft) {
						precedingFind = (FindStmt) find;
					} else {
						break;
					}
				}
			}
		}
		return precedingFind;
	}
}