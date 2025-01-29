/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.transaction;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;
import org.springframework.transaction.support.DefaultTransactionStatus;
import org.springframework.transaction.support.ResourceTransactionManager;

import com.orientechnologies.orient.core.db.ODatabaseSession;
import com.orientechnologies.orient.core.tx.OTransaction;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.commons.core.SessionManager;

/**
 * Generic class to handle Orient transactions.
 */
public class OrientTransactionManager extends AbstractPlatformTransactionManager
implements ResourceTransactionManager, InitializingBean {
	
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(OrientTransactionManager.class);
	
	@Autowired
	private transient SessionManager sessionManager;
	
	@Override
	public void afterPropertiesSet() throws Exception {
		LOG.debug(() -> "afterPropertiesSet: " + Thread.currentThread().getName());
	}

	@Override
	public Object getResourceFactory() {
		LOG.debug(() -> "get resource factory: " + Thread.currentThread().getName());
		return sessionManager;
	}

	@Override
	protected  Object doGetTransaction() {
		LOG.debug(() -> "get transaction: " + Thread.currentThread().getName() + " " + debugGetNestedTransactions());
		return getSession();
	}

	@Override
	protected void doBegin(final Object transaction, final TransactionDefinition definition) {
		getSession().begin();
		LOG.debug(() -> "begin transaction: " + Thread.currentThread().getName() + " " + debugGetNestedTransactions());
	}

	@Override
	protected void doCommit(final DefaultTransactionStatus status) {
		LOG.debug("commit closing: " + Thread.currentThread().getName() + " " + debugGetNestedTransactions());
		getSession().commit();
	}

	@Override
	protected void doRollback(final DefaultTransactionStatus status) {
		LOG.debug(() -> "rollback closing: " + Thread.currentThread().getName() + " " + debugGetNestedTransactions());
		getSession().rollback();
	}
	
	@Override
	protected void doCleanupAfterCompletion(final Object transaction) {
		if (getSession().getTransaction().amountOfNestedTxs() == 0) {
			sessionManager.getThreadDatabase().getLocalCache().clear();
			sessionManager.closeConnection();
		}
	}
	
	private ODatabaseSession getSession() {
		return sessionManager.getThreadDatabase();
	}

	private String debugGetNestedTransactions() {
		final OTransaction transaction = getSession().getTransaction();
		if (transaction == null) {
			return "(no active transaction)";
		} else {
			return "(" + transaction.amountOfNestedTxs() + " nested transactions)";
		}
	}
}
