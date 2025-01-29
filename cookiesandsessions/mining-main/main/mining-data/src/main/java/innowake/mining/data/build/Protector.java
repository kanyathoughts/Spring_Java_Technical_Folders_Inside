/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.build;

import innowake.build.obfuscation.Mode;

/**
 * Used to protect classes from obfuscation
 */
public class Protector extends innowake.build.obfuscation.Protector {

	@Override
	 protected  void declare() {
		/* spring-data relies on reflection to find the actual getter/setter, based on the fields name */
		declare("innowake.mining.data.model.springdata.**", Mode.DONT_TOUCH);
	}

}