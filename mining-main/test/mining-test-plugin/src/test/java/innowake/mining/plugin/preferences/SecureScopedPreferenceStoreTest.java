/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test for the {@link SecureScopedPreferenceStore}.
 * 
 * If you encounter a StorageException when executing this test,
 * please try adding 
 * -eclipse.password ./src/test/resources/.eclipse_master_password
 * to your run config Program arguments.
 */
public class SecureScopedPreferenceStoreTest {
	private static final String KEY = "THIS_IS_MY_KEY";
	private static final String VALUE = "THIS_IS_THE_VALUE";
	
	private SecureScopedPreferenceStore store = new SecureScopedPreferenceStore(InstanceScope.INSTANCE, "test");
	
	@Before
	public void setup() throws Exception {
		store.removeSecureValue(KEY);
	}
	
	@After
	public void tearDown() throws Exception {
		store.removeSecureValue(KEY);
	}
	
	@Test
	public void storeSecureStringTest() throws Exception {	
		assertFalse(store.containsSecure(KEY));
		
		store.setSecureValue(KEY, VALUE);
		
		assertTrue(store.containsSecure(KEY));
		assertEquals(VALUE, store.getSecureString(KEY));
	}
	
	public void removeSecureValueTest() throws Exception {
		assertFalse(store.containsSecure(KEY));
		
		store.setValue(KEY, VALUE);
		
		assertTrue(store.containsSecure(KEY));
		
		store.removeSecureValue(KEY);
		
		assertFalse(store.containsSecure(KEY));
		assertFalse(store.isSecureDefault(KEY));
	}
	
	@Test
	public void secureStringIsStoredInSeparateStoreTest() throws Exception {
		assertTrue(store.isSecureDefault(KEY));
		assertFalse(store.containsSecure(KEY));
		assertFalse(store.contains(KEY));
		
		store.setSecureValue(KEY, VALUE);
		
		assertFalse(store.isSecureDefault(KEY));
		assertTrue(store.containsSecure(KEY));
		assertFalse(store.contains(KEY));
	}
	
	@Test
	public void storeSecureDefaultStringTest() throws Exception {
		assertTrue(store.isSecureDefault(KEY));
		assertFalse(store.containsSecure(KEY));
		
		store.setSecureDefault(KEY, VALUE);
		
		assertFalse(store.containsSecure(KEY));
		assertTrue(store.isSecureDefault(KEY));
		assertEquals(VALUE, store.getDefaultSecureString(KEY));
	}
	
	@Test
	public void defaultSecureIsStoredInSeparateStoreTest() throws Exception {
		assertFalse(store.containsSecure(KEY));
		assertFalse(store.contains(KEY));
		
		store.setSecureDefault(KEY, VALUE);
		
		assertFalse(store.containsSecure(KEY));
		assertFalse(store.contains(KEY));		
	}
	
	@Test
	public void setToSecureDefaultTest() throws Exception {
		assertTrue(store.isSecureDefault(KEY));
		
		store.setSecureValue(KEY, VALUE);
		
		assertFalse(store.isSecureDefault(KEY));
		
		store.setToSecureDefault(KEY);
		
		assertTrue(store.isSecureDefault(KEY));
	}
	
	@Test
	public void moveToSecureTest() throws Exception {
		store.setValue(KEY, VALUE);
		
		assertTrue(store.contains(KEY));
		assertFalse(store.containsSecure(KEY));
		
		store.moveStringToSecure(KEY);
		
		assertFalse(store.contains(KEY));
		assertTrue(store.containsSecure(KEY));
	}

	@Test
	public void needsSavingTest() throws Exception {
		assertFalse(store.needsSaving());
		
		store.setSecureValue(KEY, VALUE);
		
		assertTrue(store.needsSaving());
		
		store.save();
		
		assertFalse(store.needsSaving());
	}

	@Test
	public void removeValueTest() throws Exception {
		assertFalse(store.contains(KEY));
		
		store.setSecureValue(KEY, VALUE);
		
		assertTrue(store.containsSecure(KEY));
		
		store.removeSecureValue(KEY);
		
		assertFalse(store.containsSecure(KEY));
	}
}
