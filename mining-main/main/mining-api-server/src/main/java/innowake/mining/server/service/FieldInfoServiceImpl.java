/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.postgres.FieldInfoPgDao;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Functions for accessing {@code field_info} entities.
 */
@Service
public class FieldInfoServiceImpl implements FieldInfoService {

	private final FieldInfoPgDao fieldInfoDao;

	@Autowired
	public FieldInfoServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		fieldInfoDao = new FieldInfoPgDao(jdbcTemplate);
	}

	@Override
	public UUID create(final FieldInfoPojoPrototype fieldInfo) {
		return fieldInfoDao.put(fieldInfo, true);
	}

	@Override
	public void update(final FieldInfoPojoPrototype fieldInfo) {
		fieldInfoDao.put(fieldInfo, false);
	}
	
	@Override
	public int delete(final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return fieldInfoDao.delete(builder);
	}

	@Override
	public List<FieldInfoPojo> find(final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return fieldInfoDao.find(builder);
	}

	@Override
	public Paged<FieldInfoPojo> find(final Pagination paging, final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return fieldInfoDao.find(paging, builder);
	}

	@Override
	public Optional<FieldInfoPojo> findAny(final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return fieldInfoDao.findAny(builder);
	}
}
