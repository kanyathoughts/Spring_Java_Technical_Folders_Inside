/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.custom.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Annotation that can be used to either suppress serialization of
 * properties (during serialization), or ignore processing of
 * JSON properties read (during deserialization).
 *<p>
 * Example:
 *<pre>
 * // to prevent specified fields from being serialized or deserialized
 * // (i.e. not include in JSON output; or being set even if they were included)
 * &#064;JsonIgnoreUnknownProperties({ "internalId", "secretKey" })
 * // To ignore any unknown properties in JSON input without exception:
 * &#064;JsonIgnoreUnknownProperties(ignoreUnknown=true)
 *</pre>
 *<p>
 * Annotation can be applied both to classes and
 * to properties. If used for both, actual set will be union of all
 * ignorals: that is, you can only add properties to ignore, not remove
 * or override. So you can not remove properties to ignore using
 * per-property annotation.
 */
@Target({ElementType.ANNOTATION_TYPE, ElementType.TYPE,
    ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@JsonIgnoreProperties(ignoreUnknown = true)
public @interface JsonIgnoreUnknownProperties {}
