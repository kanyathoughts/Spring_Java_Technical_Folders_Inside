/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.build;

import innowake.build.obfuscation.Mode;

/**
 * Used to protect classes from obfuscation
 */
public class Protector extends innowake.build.obfuscation.Protector {

	@Override
	 protected  void declare() {
		declare("innowake.mining.data.core.api.**", Mode.DONT_TOUCH);
		declare("innowake.mining.data.core.storeast.api.**", Mode.DONT_TOUCH);
		declare("innowake.mining.data.core.taxonomy.api.**", Mode.DONT_TOUCH);
		declare("innowake.mining.data.core.annotation.api.**", Mode.DONT_TOUCH);
		declare("innowake.mining.data.core.datadictionary.api.**", Mode.DONT_TOUCH);
	}

}