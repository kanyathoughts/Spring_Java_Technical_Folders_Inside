/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static java.lang.String.format;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import innowake.mining.shared.Versions;

/**
 * Test for checking release versions with {@link Versions}.
 */
@RunWith(Parameterized.class)
public class VersionsTest {
	
	@Parameters(name = "{index}: {0} == {1} ({2})")
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
                 { "99.9.99.MINING-SNAPSHOT", "99.9.99-TRUNK-MINING-SNAPSHOT", TRUE }, 
                 { "99.9.99.MINING", "99.9.99-TRUNK-MINING-SNAPSHOT", FALSE },
                 { "99.9.99.MINING-SNAPSHOT", "99.9.99-TRUNK-MINING", FALSE }, 
                 { "1.2.3.MINING-SNAPSHOT", "4.5.6-TRUNK-MINING-SNAPSHOT", FALSE }, 
                 { "19.5.00.alpha-202012150357", "19.5.00-alpha-202012150357-25", TRUE }, 
                 { "19.5.00.beta-202012150357-25-foo-bar-bazquax", "19.5.00-beta-202012150357-25", TRUE }, 
                 { "19.5.10", "19.5.10", TRUE }, 
                 { "19.6.00", "19.6.00", TRUE }, 
                 { "20.5.00", "20.5.00", TRUE }, 
                 { "19.5.00", "19.5.10", FALSE }, 
                 { "19.6.00", "19.5.00", FALSE }, 
                 { "20.6.00", "19.6.00", FALSE }, 
           });
    }
    
    private final String versionA;
	private final String versionB;
	private final Boolean expected;
    
	/**
	 * Creates a new test instance with the given parameters.
	 * 
	 * @param versionA the first test string
	 * @param versionB the second test string
	 * @param expected the expected equality result
	 */
	public VersionsTest(final String versionA, final String versionB, final Boolean expected) {
		this.versionA = versionA;
		this.versionB = versionB;
		this.expected = expected;
	}
	
	/**
	 * Based on the expected result value it is asserted that the given parameters are symmetrical (un)equal.
	 */
	@Test
	public void equalityTest() {
		if (expected.booleanValue()) {
			assertTrue(format("%s and %s must be equal", versionA, versionB), Versions.equals(versionA, versionB));
			assertTrue(format("%s and %s must be equal", versionB, versionA), Versions.equals(versionB, versionA));
		} else {
			assertFalse(format("%s and %s must not be equal", versionA, versionB), Versions.equals(versionA, versionB));
			assertFalse(format("%s and %s must not be equal", versionB, versionA), Versions.equals(versionB, versionA));
		}
	}

}
