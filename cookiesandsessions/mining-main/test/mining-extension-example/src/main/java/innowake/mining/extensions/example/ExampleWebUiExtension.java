/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.example;

import innowake.mining.shared.extensions.MiningWebUiExtension;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 * Example implementation of {@link MiningWebUiExtension}
 */
@Component
public class ExampleWebUiExtension implements MiningWebUiExtension {

	@Override
	public String getName() {
		return "Example UI Extension";
	}

	@Override
	public String getPageIdentifier() {
		return "example-ui";
	}

	@Override
	public Kind getKind() {
		return Kind.IFRAME;
	}



	@Override
	public Map<Property, String> getProperties() {
		final Map<Property, String> props = new HashMap<>();
		props.put(Property.IFRAME_SRC, "/my/extension-url/example-ui.html");
		return props;
	}

}
