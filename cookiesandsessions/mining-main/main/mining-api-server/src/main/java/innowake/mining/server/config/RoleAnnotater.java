/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springdoc.core.customizers.OperationCustomizer;
import org.springframework.boot.autoconfigure.security.SecurityProperties;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import io.swagger.v3.oas.models.Operation;

/**
 * Prepends the role information to the notes section of the {@link Operation} based on the security annotations.
 * <p>
 * As the {@link OperationCustomizer} uses an order of {@value SecurityProperties#BASIC_AUTH_ORDER} we need
 * to increase our order accordingly, otherwise there's the risk of the notes section being overwritten.
 */
@Component
@Order(SecurityProperties.BASIC_AUTH_ORDER + 1)
public class RoleAnnotater implements OperationCustomizer {
	
	@Override
	public Operation customize(final Operation operation, final HandlerMethod handlerMethod) {
		
		final Optional<Nature> nature = Optional.ofNullable(handlerMethod.getMethodAnnotation(Nature.class));
        final Optional<Role> role = Optional.ofNullable(handlerMethod.getMethodAnnotation(Role.class));

        if ( ! nature.isPresent() || ! role.isPresent()) {
            return operation;
        }
		
		final String natureTypes = Arrays.stream(nature.get().value()).map(NatureType::getValue).collect(Collectors.joining(","));
		final String roleTypes = Arrays.stream(role.get().value()).map(RoleType::getValue).collect(Collectors.joining(","));
		final String existingNotes = operation.getSummary();
		/* Prepend the role information to the existing notes */
		final String notes = String.format("User Role(s): %s | Project Nature(s): %s\n\n%s", roleTypes, natureTypes, existingNotes);
		
		/* Set the updated notes */
		operation.description(notes);
		
		return operation;
	}

}
