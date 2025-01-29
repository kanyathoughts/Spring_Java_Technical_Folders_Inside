package innowake.mining.server.graphql;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;

import graphql.Internal;
import graphql.language.Value;
import graphql.scalars.datetime.DateTimeScalar;
import graphql.schema.Coercing;
import graphql.schema.CoercingParseLiteralException;
import graphql.schema.CoercingParseValueException;
import graphql.schema.CoercingSerializeException;
import graphql.schema.GraphQLScalarType;
import innowake.lib.core.api.lang.Nullable;

/**
 * Decorator for {@link DateTimeScalar#INSTANCE} that adds {@code java.time.Instant} conversion capabilities. 
 */
@Internal
public final class GraphQLInstantDateTimeScalar {

    public static final GraphQLScalarType INSTANCE;

    private GraphQLInstantDateTimeScalar() {}

    static {
        final Coercing<OffsetDateTime, String> coercing = new Coercing<OffsetDateTime, String>() {
            
            @SuppressWarnings("unchecked")
            private final Coercing<OffsetDateTime, String> PARENT_INSTANCE = (Coercing<OffsetDateTime, String>) DateTimeScalar.INSTANCE.getCoercing();

            @Override
            public String serialize(@Nullable final Object input) throws CoercingSerializeException {
                return PARENT_INSTANCE.serialize(convertInstantToOffsetDateTime(input));
            }

            @Override
            public OffsetDateTime parseValue(@Nullable final Object input) throws CoercingParseValueException {
                return PARENT_INSTANCE.parseValue(convertInstantToOffsetDateTime(input));
            }

            @Override
            public OffsetDateTime parseLiteral(@Nullable final Object input) throws CoercingParseLiteralException {
                return PARENT_INSTANCE.parseLiteral(input);
            }

            @Override
            public Value<?> valueToLiteral(@Nullable final Object input) {
                return PARENT_INSTANCE.valueToLiteral(input);
            }

            @Nullable
            private Object convertInstantToOffsetDateTime(@Nullable final Object input) {
                final Object fixedInput;
                if (input instanceof Instant) {
                    fixedInput = ((Instant) input).atOffset(ZoneOffset.UTC);
                } else {
                    fixedInput = input;
                }
                return fixedInput;
            }
        };

        INSTANCE = GraphQLScalarType.newScalar()
                .name("DateTime")
                .description("An RFC-3339 compliant DateTime Scalar also accepting java.time.Instant")
                .coercing(coercing)
                .build();
    }
}
