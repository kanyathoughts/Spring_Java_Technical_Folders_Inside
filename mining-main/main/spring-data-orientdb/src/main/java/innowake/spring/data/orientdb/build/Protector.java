/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.build;

import innowake.build.obfuscation.Mode;

/**
 * Used to protect classes from obfuscation
 */
public class Protector extends innowake.build.obfuscation.Protector {

	@Override
	 protected  void declare() {
		declare("innowake.spring.data.orientdb.api.**", Mode.DONT_TOUCH);
	}

}