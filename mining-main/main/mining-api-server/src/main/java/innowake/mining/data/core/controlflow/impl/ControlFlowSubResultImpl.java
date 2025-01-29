/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.AbstractMap;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.collections4.set.UnmodifiableSet;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Contains the statements where following the control flow ended.
 * @param <S> statement type
 */
public class ControlFlowSubResultImpl<S extends Statement> implements ControlFlowSubResult<S> {
	
	private final Set<Entry<S, String>> lastSimpleStatements = new HashSet<>();
	private final Set<Entry<Entry<S, String>, AstNodePojo>> lastUnhandledStatements = new HashSet<>();

	@Override
	public void addLastSimpleStatement(final S lastStatement) {
		addLastSimpleStatement(lastStatement, null);
	}

	@Override
	public void addLastSimpleStatement(S statement, @Nullable String label) {
		lastSimpleStatements.add(new AbstractMap.SimpleEntry<>(statement, label));
	}

	@Override
	public void addLastUnhandledStatement(S statement, AstNodePojo section) {
		addLastUnhandledStatement(statement, section, null);
	}
	
	@Override
	public void addLastUnhandledStatement(S statement, AstNodePojo section, @Nullable String label) {
		lastUnhandledStatements.add(new AbstractMap.SimpleEntry<>(new AbstractMap.SimpleEntry<>(statement, label), section));
	}
	
	@Override
	public Set<Entry<S, String>> getLastSimpleStatements() {
		return UnmodifiableSet.unmodifiableSet(lastSimpleStatements);
	}
	
	@Override
	public Set<Entry<Entry<S, String>, AstNodePojo>> getLastUnhandledStatements() {
		return UnmodifiableSet.unmodifiableSet(lastUnhandledStatements);
	}

}
