/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.build;

import innowake.build.obfuscation.Mode;

/**
 * Used to protect classes from obfuscation
 */
public class Protector extends innowake.build.obfuscation.Protector {

	@Override
	 protected  void declare() {
		declare("innowake.lib.job.api.**", Mode.DONT_TOUCH);
		
		/* will lead to serialization issues when these classes are obfuscated */
		declare("innowake.lib.job.internal.hazelcast.**", Mode.DONT_TOUCH);
	}

}