/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.tuple.Triple;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.postgres.DataFlowPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Access to data flow related data.
 */
@Service
public class DataFlowServiceImpl implements DataFlowService {

	private final DataFlowPgDao dao;
	private final ModulePgDao moduleDao;

	@Autowired
	public DataFlowServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		this.dao = new DataFlowPgDao(jdbcTemplate);
		moduleDao = new ModulePgDao(jdbcTemplate);
	}

	@Override
	public UUID create(final DataFlowNodePojoPrototype node) {
		return dao.put(node, true);
	}

	@Override
	public void update(final DataFlowNodePojoPrototype node) {
		dao.put(node, false);
	}

	@Override
	public void update(final BuildingConsumer<DataFlowNodeInquiryBuilder> builder, final DataFlowNodePojoPrototype node) {
		dao.update(builder, node);
	}

	@Override
	public DataFlowNodePojo get(final UUID uid) {
		return findAny(q -> q.byId(uid))
					.orElseThrow(() -> new MiningEntityNotFoundException(DataFlowNodePojo.class, uid.toString()));
	}

	@Override
	public List<DataFlowNodePojo> find(final BuildingConsumer<DataFlowNodeInquiryBuilder> builder) {
		return dao.find(builder);
	}

	@Override
	public List<UUID> findIds(final BuildingConsumer<DataFlowNodeInquiryBuilder> builder) {
		return dao.findIds(builder);
	}
	
	@Override
	public Optional<DataFlowNodePojo> findAny(final BuildingConsumer<DataFlowNodeInquiryBuilder> builder) {
		return dao.findAny(builder);
	}

	@Override
	public Optional<UUID> findAnyId(final BuildingConsumer<DataFlowNodeInquiryBuilder> builder) {
		return dao.findAnyId(builder);
	}

	@Override
	public void createRelationships(final Collection<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships) {
		dao.createRelationships(relationships);
	}

	@Override
	public UUID createProxyContainer(final ProxyContainerPojoPrototype proto) {
		return dao.put(proto, true);
	}

	@Override
	public void updateProxyContainer(final ProxyContainerPojoPrototype proto) {
		dao.put(proto, false);
	}

	@Override
	public List<ProxyContainerPojo> findProxyContainers(final BuildingConsumer<ProxyContainerInquiryBuilder> builder) {
		return dao.findProxyContainers(builder);
	}

	@Override
	public ProxyContainerPojo getProxyContainer(final UUID uid) {
		return findAnyProxyContainer(q -> q.byId(uid))
				.orElseThrow(() -> new MiningEntityNotFoundException(ProxyContainerPojo.class, String.valueOf(uid)));
	}

	@Override
	public Optional<ProxyContainerPojo> findAnyProxyContainer(final BuildingConsumer<ProxyContainerInquiryBuilder> builder) {
		return dao.findAnyProxyContainer(builder);
	}

	@Override
	public void createProxyContainerRelationships(final UUID proxyContainerUid, final List<UUID> nodeUid) {
		dao.createProxyContainerRelationships(proxyContainerUid, nodeUid);
	}

	@Override
	public int deleteProxyContainerRelationships(final UUID proxyContainerUid) {
		return dao.deleteProxyContainerRelationships(proxyContainerUid);
	}

	@Override
	public UUID createError(final DataFlowErrorPojoPrototype error) {
		return dao.createError(error);
	}

	@Override
	public void createErrors(final Collection<DataFlowErrorPojoPrototype> protos) {
		dao.createErrors(protos);
	}

	@Override
	public int deleteErrors(final BuildingConsumer<DataFlowErrorInquiryBuilder> builder) {
		return dao.deleteErrors(builder);
	}

	@Override
	public List<DataFlowErrorPojo> findErrors(final BuildingConsumer<DataFlowErrorInquiryBuilder> builder) {
		return dao.findErrors(builder);
	}

	@Override
	public void deleteForModule(final EntityId module) {
		moduleDao.removeModuleInfoProperty(q -> q.byId(module), DataLineageCoreService.TRACE_EXECUTED_PROPERTY);
		dao.setTraced(module, false);
		dao.deleteNodes(q -> q.ofModule(module));
		dao.deleteProxyContainers(q -> q.ofModule(module));
	}

	@Override
	public void deleteForProject(final EntityId project) {
		moduleDao.removeModuleInfoProperty(q -> q.ofProject(project), DataLineageCoreService.TRACE_EXECUTED_PROPERTY);
		dao.deleteNodes(q -> q.ofProject(project));
		dao.deleteProxyContainers(q -> q.ofProject(project));
	}

}