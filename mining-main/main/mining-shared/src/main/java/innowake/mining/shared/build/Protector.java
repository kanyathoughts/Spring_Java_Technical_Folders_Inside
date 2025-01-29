/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.build;

import innowake.build.obfuscation.Mode;

/**
 * Used to protect classes from obfuscation
 */
public class Protector extends innowake.build.obfuscation.Protector {

	@Override
	 protected  void declare() {
		/* must not be obfuscated because of various utilization of XML and JSON (de-)serialization */
		declare("innowake.mining.shared.**", Mode.DONT_TOUCH);
	}

}