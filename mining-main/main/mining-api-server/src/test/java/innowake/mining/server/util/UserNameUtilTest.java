/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.authorization.AbstractUserNameTest;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.model.UserIdentifyable;

/**
 * Tests for the {@link UserNameUtil}.
 */
class UserNameUtilTest extends AbstractUserNameTest {

	@Autowired
	private UserNameUtil userNameUtil;

	/**
	 * Tests the {@link UserNameUtil#fillUserNames(Iterable)}
	 */
	@Test
	void testFillUserNames() {
		mockResponseExpectOk(10);

		final List<AnnotationPojo> annotations = new ArrayList<>();
		for (int i = 1; i <= 10; i++) {
			final String numberI = String.valueOf(i);
			annotations.add(new AnnotationPojoDummy().prepare(a -> a
					.setCreatedByUserId(numberI)
					.setUpdatedByUserId(numberI)
				).build());
		}
		annotations.forEach(a -> {
			a.setCreatedByUserName(userNameUtil::getUserName);
			a.setUpdatedByUserName(userNameUtil::getUserName);
		});
		for (final AnnotationPojo annotation : annotations) {
			final String createdBy = annotation.getCreatedByUserName().orElse(null);
			assertEquals(String.format("FName-%s LName-%s", annotation.getCreatedByUserId(), annotation.getCreatedByUserId()), createdBy);
			final String updatedBy = annotation.getUpdatedByUserName().orElse(null);
			assertEquals(String.format("FName-%s LName-%s", annotation.getUpdatedByUserId().get(), annotation.getUpdatedByUserId().get()), updatedBy);
		}
	}

	/**
	 * Tests {@link UserNameUtil#fillUserName(UserIdentifyable)} - a valid user.
	 */
	@Test
	void testFillUserName() {
		mockResponseExpectOk(2);

		final AnnotationPojo annotation = new AnnotationPojoDummy().prepare(a -> a
				.setCreatedByUserId("2")
				.setUpdatedByUserId("1")
			).build();
		annotation.setCreatedByUserName(userNameUtil::getUserName);
		annotation.setUpdatedByUserName(userNameUtil::getUserName);
		assertEquals("FName-2 LName-2", annotation.getCreatedByUserName().orElse(null));
		assertEquals("FName-1 LName-1", annotation.getUpdatedByUserName().orElse(null));
	}
	
	/**
	 * Test for Blank user id.
	 */
	@Test
	void testFillUserNameEmptyId() {
		final AnnotationPojo annotation = new AnnotationPojoDummy().prepare(a -> a
				.setCreatedByUserId("")
				.setUpdatedByUserId(" ")
			).build();
		annotation.setCreatedByUserName(userNameUtil::getUserName);
		annotation.setUpdatedByUserName(userNameUtil::getUserName);
		assertEquals("", annotation.getCreatedByUserName().orElse(null));
		assertEquals("", annotation.getUpdatedByUserName().orElse(null));
	}

	/**
	 * Tests {@link UserNameUtil#fillUserName(UserIdentifyable)} - a invalid user(missing user info in keycloak).
	 */
	@Test
	void testFillUserNameMissingId() {
		mockResponseExpectNotFound("3");

		final AnnotationPojo annotation = new AnnotationPojoDummy().prepare(a -> a
				.setCreatedByUserId("3")
				.setUpdatedByUserId("3")
			).build();
		annotation.setCreatedByUserName(userNameUtil::getUserName);
		annotation.setUpdatedByUserName(userNameUtil::getUserName);
		assertEquals("Unknown User", annotation.getCreatedByUserName().orElse(null));
		assertEquals("Unknown User", annotation.getUpdatedByUserName().orElse(null));
	}
}
