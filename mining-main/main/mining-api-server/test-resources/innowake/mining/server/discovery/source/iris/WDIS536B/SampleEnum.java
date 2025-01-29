/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package WDIS536B.test.sample;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import innowake.mining.shared.model.TypeIdentifier;
import innowake.spring.data.orientdb.api.annotations.Entity;

/**
 * The type associated with a module.
 */
@Entity(name = "TypeEnum")
public enum SampleEnum {
	SAMPLE_ENTRY
}
