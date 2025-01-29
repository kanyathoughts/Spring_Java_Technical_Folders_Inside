/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectRole;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests handling the differences of the JSON content between the web UI and the Java based API client.
 * <p>
 * The Java based API client uses a non-standard configuration of the Jackson {@link ObjectMapper} which leads
 * to different JSON content than the one created e.g. by the web UI.
 * <p>
 * As this can lead to (de)serialization issues if not properly handled, the tests included here do not
 * use any special configuration, which should closely mimic the way Spring and the web UI interact.
 */
public class JsonSerializationTest {
	
	/**
	 * Test ensuring the proper JSON (de)serialization of {@links ModulePojo ModulePojos}.
	 * <p>
	 * Does not validate the actual content, only makes sure that there are no exceptions during (de)serializing.
	 *
	 * @throws JsonProcessingException if the JSON processing is not successful
	 */
	@Test
	public void modulesCanBeSerialized() throws JsonProcessingException {
		/* Create a minimal valid Module with regards to serialization */
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setUid(new UUID(0, 0));
		module.setNid(Long.valueOf(1));
		module.setName("name");
		module.setProject(EntityId.of(Long.valueOf(1)));
		module.setTechnology(Technology.ASSEMBLER);
		module.setType(Type.ADAPTER);
		module.setStorage(Storage.DATABASE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		
		/* Serialize the Module */
		final String jsonString = PojoMapper.jsonWriter().writeValueAsString(module);
		
		/* Deserialize the Module */
		PojoMapper.jsonReaderFor(ModulePojoPrototype.class).readValue(jsonString);
	}
	
	/**
	 * To test toString method of Member.java in case where id is null.
	 *
	 * @throws JsonProcessingException if the JSON processing is not successful
	 */
	@Test
	public void testMemberToString() throws JsonProcessingException {
		/* Deliberately not setting id in jsonInput to check toString method. This should not throw null pointer in case where id is null*/
		final String jsonInput = "{\"email\":\"test@in.com\"}";
		final Member member = PojoMapper.jsonReaderFor(Member.class).readValue(jsonInput);
		final String output = member.toString();
		assertFalse("The toString method has returned empty result, this should not be the case", StringUtils.isEmpty(output));
		assertNull(member.getId());
	}
	
	/**
	 * To test the equals method of Member.java should not throw null pointer exception even though the id in member object is null.
	 *
	 * @throws JsonProcessingException if the JSON processing is not successful
	 */
	@Test
	public void testMemberEquals() throws JsonProcessingException {
		/* Deliberately not setting id in jsonInput to check equals method. This should not throw null pointer in case where id is null*/
		final String jsonInput = "{\"email\":\"test@in.com\"}";
		final Member member = PojoMapper.jsonReaderFor(Member.class).readValue(jsonInput);
		final Member member2 = new Member("1", "First", "Last", "test@in.com", new ArrayList<ProjectRole>());
		assertFalse("The equals method has returned true, this should not be the case", member.equals(member2));
		assertNull(member.getId());
	}

}
