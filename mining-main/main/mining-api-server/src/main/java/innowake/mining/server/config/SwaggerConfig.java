package innowake.mining.server.config;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;

import com.fasterxml.jackson.databind.type.SimpleType;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import io.swagger.v3.core.converter.AnnotatedType;
import io.swagger.v3.core.converter.ModelConverter;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.UUIDSchema;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;

/**
 * The Swagger Spring configuration.
 */
@Configuration
@EnableWebSecurity
public class SwaggerConfig {

	private static final String API_KEY_NAME = "access_token";
	
	@Autowired
	private BuildProperties buildProperties;

	private SecurityScheme apiKey() {
		return new SecurityScheme().name(API_KEY_NAME).type(SecurityScheme.Type.APIKEY).in(SecurityScheme.In.QUERY);
	}

	@Bean
	public OpenAPI userApi() {
		return new OpenAPI().components(new Components()
				.addSecuritySchemes(API_KEY_NAME, apiKey()))
				.addSecurityItem(new SecurityRequirement().addList(API_KEY_NAME))
				.info(apiInfo());
	}
	
	private Info apiInfo() {
		return new Info()
			.title("Mining Server REST API")
			.description("This is the description of the mining server REST API endpoints.")
			.version(buildProperties.getVersion());
	}

	/**
	 * Model converter for unwrapping the element type of  {@link Definable} in the OpenAPI schema.
	 * <p>
	 * {@code Definable<Foo>} is represented as just {@code Foo} in the schema.
	 *
	 * @return the converter bean
	 */
	@Bean
	@Order /* has Ordered.LOWEST_PRECEDENCE by default */
	public ModelConverter definableConverter() {
		return (type, context, chain) -> {
			if (type.getType() instanceof ParameterizedType) {
				final var parameterizedType = (ParameterizedType) type.getType();
				if (Definable.class.isAssignableFrom((Class<?>) parameterizedType.getRawType())) {
					return context.resolve(createAnnotatedType(type, parameterizedType.getActualTypeArguments()[0]));
				}
			}
			if (type.getType() instanceof SimpleType) {
				final var simpleType = (SimpleType) type.getType();
				if (Definable.class.isAssignableFrom(simpleType.getRawClass())) {
					return context.resolve(createAnnotatedType(type, simpleType.getBindings().getTypeParameters().get(0)));
				}
			}

			return chain.hasNext() ? chain.next().resolve(type, context, chain) : null;
		};
	}

	private AnnotatedType createAnnotatedType(final AnnotatedType originalType, final Type valueType) {
		/* clones the originalType with the new valueType */
		return new AnnotatedType()
				.type(valueType)
				.schemaProperty(originalType.isSchemaProperty())
				.ctxAnnotations(originalType.getCtxAnnotations())
				.skipSchemaName(originalType.isSkipSchemaName())
				.resolveAsRef(originalType.isResolveAsRef())
				.propertyName(originalType.getPropertyName())
				.jsonViewAnnotation(originalType.getJsonViewAnnotation())
				.parent(originalType.getParent());
	}

	/**
	 * Model converter for correctly representing {@link innowake.mining.shared.access.EntityId} in the OpenAPI schema.
	 * <p>
	 * For Entity ids, we can accept either a number ({@linkplain EntityId#EntityId(Long) nid}) or UUID ({@linkplain EntityId#EntityId(UUID) uid}).
	 *
	 * @return the converter bean
	 */
	@SuppressWarnings("unchecked")
	@Bean
	/* the Ordered.LOWEST_PRECEDENCE - 1 is required so the entityIdConverter is applied AFTER the definableConverter,
	 * so that Definable<EntityId> is properly unwrapped to UUID and number */
	@Order(Ordered.LOWEST_PRECEDENCE - 1)
	public ModelConverter entityIdConverter() {
		return (type, context, chain) -> {
			
			if (type.getType() instanceof Class<?> && EntityId.class.isAssignableFrom((Class<?>) type.getType())
					|| type.getType() instanceof SimpleType && EntityId.class.isAssignableFrom(((SimpleType) type.getType()).getRawClass())) {
				/* Add EntityId type to model */
				if ( ! context.getDefinedModels().containsKey(EntityId.class.getSimpleName())) {
					context.defineModel(EntityId.class.getSimpleName(), 
										new Schema<>().name(EntityId.class.getSimpleName())
													  .oneOf(Arrays.asList(new UUIDSchema(), new NumberSchema())));
				}
				/* Add reference to EntityId type in model. Do not create a schema here! Otherwise the openapi generator will create a new type with the name depending
				 * on the first caller */
				return new Schema<>().$ref(EntityId.class.getSimpleName());
			}

			return chain.hasNext() ? chain.next().resolve(type, context, chain) : null;
		};
	}
}
