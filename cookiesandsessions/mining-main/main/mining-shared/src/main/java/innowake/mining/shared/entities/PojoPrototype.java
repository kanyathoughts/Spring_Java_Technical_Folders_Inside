/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.entities;

import java.io.Serializable;

import innowake.mining.shared.Definable;
import innowake.mining.shared.PojoPrototypeSerializer;

/**
 * Marker interface for classes containing {@code public} {@link Definable} attributes to be serialized to JSON
 * by {@link PojoPrototypeSerializer} like <code>{["attr1":&lt;value1&gt;][,"attr2":&lt;value2&gt;][,...]}</code>.
 */
public interface PojoPrototype extends Serializable {

}
