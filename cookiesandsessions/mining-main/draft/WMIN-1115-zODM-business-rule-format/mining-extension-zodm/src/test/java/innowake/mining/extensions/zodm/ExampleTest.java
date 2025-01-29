package innowake.mining.extensions.zodm;
import org.junit.Ignore;
import org.junit.jupiter.api.Test;

import fw2.hhs.rules.RuleManager;
import fw2.hhs.rules.RuleProject;

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */

public class ExampleTest {
	
	@Test
	@Ignore
	public void testCreateEmptyProject() {
		// TODO: this doesn't work yet = it is just here to check whether the classpath is set up correctly (i.e. whether it compiles :-))
		final RuleManager ruleManager = new RuleManager();
		final RuleProject ruleProject = new RuleProject();
		
		ruleManager.createProject(ruleProject, new String[] { "xom" });
	}
}
