/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.graphql;

import graphql.language.IntValue;
import graphql.language.NullValue;
import graphql.language.StringValue;
import graphql.language.Value;
import graphql.schema.Coercing;
import graphql.schema.CoercingParseLiteralException;
import graphql.schema.CoercingParseValueException;
import graphql.schema.CoercingSerializeException;
import graphql.schema.GraphQLScalarType;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;

import java.math.BigInteger;

import static graphql.scalars.util.Kit.typeName;

/**
 * Custom GraphQL scalar type for {@link EntityId}
 */
public class GraphQLEntityId {

	private static class EntityIdCoercing implements Coercing<EntityId, Object> {

		@Override
		@Nullable
		public Object serialize(@Nullable final Object input) {
			if (input == null) {
				return null;
			}
			if (input instanceof EntityId) {
				final EntityId entityId = (EntityId) input;
				if (entityId.hasUid()) {
					return entityId.getUid().toString();
				} else if (entityId.hasNid()) {
					return entityId.getNid();
				} else {
					return null;
				}
			}
			if (input instanceof Long) {
				return input;
			}
			throw new CoercingSerializeException("Expected EntityId but was '" + typeName(input) + "'.");
		}

		@Override
		public EntityId parseValue(@Nullable final Object input) {
			if (input instanceof String) {
				return EntityId.of((String) input);
			} else if (input instanceof Number) {
				return EntityId.of(((Number) input).longValue());
			}
			throw new CoercingParseValueException("Expected String or Number input for EntityId, but was" + typeName(input));
		}

		@Override
		public EntityId parseLiteral(@Nullable final Object input) {
			if (input instanceof StringValue) {
				return EntityId.of(((StringValue) input).getValue());
			} else if (input instanceof IntValue) {
				return EntityId.of(((IntValue) input).getValue().longValue());
			}
			throw new CoercingParseLiteralException("Expected StringValue or IntValue for EntityId, but was " + typeName(input));
		}

		@Override
		public Value<?> valueToLiteral(@Nullable final Object input) {
			final Object serialized = serialize(input);
			if (serialized instanceof String) {
				return StringValue.of((String) serialized);
			} else if (serialized instanceof Long) {
				return IntValue.newIntValue(BigInteger.valueOf((Long) serialized)).build();
			} else {
				return NullValue.of();
			}
		}
	}

	public static final GraphQLScalarType INSTANCE = GraphQLScalarType.newScalar()
			.name("EntityId")
			.description("EntityId containing either a UUID or numerical id")
			.coercing(new EntityIdCoercing())
			.build();

	private GraphQLEntityId() { }

}
