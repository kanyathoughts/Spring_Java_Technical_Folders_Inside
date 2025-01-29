/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.graphql;

import static graphql.scalars.util.Kit.typeName;

import java.math.BigInteger;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.Date;

import graphql.language.IntValue;
import graphql.language.Value;
import graphql.scalars.java.JavaPrimitives;
import graphql.schema.Coercing;
import graphql.schema.CoercingSerializeException;
import graphql.schema.GraphQLScalarType;
import innowake.lib.core.api.lang.Nullable;

/**
 * Custom GraphQL scalar type for a timestamp in milliseconds
 */
public class GraphQLTimestamp {

	private static class TimestampCoercing implements Coercing<Instant, Long> {
		@Override
		public Long serialize(@Nullable final Object input) {
			final Instant t;
			if (input instanceof Instant) {
				t = (Instant) input;
			} else if (input instanceof Date) {
				t = ((Date) input).toInstant();
			} else if (input instanceof OffsetDateTime) {
				t = ((OffsetDateTime) input).toInstant();
			} else if (input instanceof ZonedDateTime) {
				t = ((ZonedDateTime) input).toInstant();
			} else {
				throw new CoercingSerializeException(
						"Expected Java Date, Instant, OffsetDateTime or ZonedDateTime but was '" + typeName(input) + "'."
					);
			}
			return Long.valueOf(t.toEpochMilli());
		}

		@Override
		public Instant parseValue(@Nullable final Object input) {
			return Instant.ofEpochMilli(((Long) JavaPrimitives.GraphQLLong.getCoercing().parseValue(input)).longValue());
		}

		@Override
		public Instant parseLiteral(@Nullable final Object input) {
			return Instant.ofEpochMilli(((Long) JavaPrimitives.GraphQLLong.getCoercing().parseLiteral(input)).longValue());
		}

		@Override
		public Value<?> valueToLiteral(@Nullable final Object input) {
			return IntValue.newIntValue(BigInteger.valueOf(serialize(input).longValue())).build();
		}
	}
	
	public static final GraphQLScalarType INSTANCE = GraphQLScalarType.newScalar()
			.name("Timestamp")
			.description("Milliseconds from the epoch of 1970-01-01T00:00:00Z")
			.coercing(new TimestampCoercing())
			.build();

	private GraphQLTimestamp() { }

}
