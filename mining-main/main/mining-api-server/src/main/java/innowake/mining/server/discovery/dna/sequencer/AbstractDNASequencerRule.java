/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;

public abstract class AbstractDNASequencerRule<T> {

	/**
	 * Method to handle the Exec Sql nodes
	 *
	 * @param model The parser node model
	 * @param collector The DNA collector
	 */
	protected void handleBaseExecSql(final @Nullable ExecNode<?> model, final DNACollector<T> collector) {
		if (model != null) {
			final String simpleName = model.getClass().getSimpleName();
			final String category = findExecCategory(simpleName);
			if (category != null) {
				collector.add(() -> category, model);
			}
		}
	}

	private @Nullable String findExecCategory(final String simpleName) {
		String resultCategory = null;
		switch (simpleName) {
			case "ExecSqlSelect":
				resultCategory = "select";
				break;
			case "ExecSqlInsert":
				resultCategory = "insert";
				break;
			case "ExecSqlUpdate":
				resultCategory = "update";
				break;
			case "ExecSqlDelete":
				resultCategory = "delete";
				break;
			case "ExecSqlRollback":
				resultCategory = "rollback";
				break;
			case "ExecSqlCommit":
				resultCategory = "commit";
				break;
			case "ExecSqlFetch":
				resultCategory = "fetch";
				break;
			case "ExecSqlSet":
				resultCategory = "set";
				break;
			case "ExecSqlClose":
				resultCategory = "close";
				break;
			default:
				break;
		}
		return resultCategory;
	}

}
