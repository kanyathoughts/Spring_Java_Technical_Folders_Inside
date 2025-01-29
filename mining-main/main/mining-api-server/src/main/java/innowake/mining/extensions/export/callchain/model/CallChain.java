package innowake.mining.extensions.export.callchain.model;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.shared.entities.ModuleLightweightPojo;

/**
 * The {@code Call Chain} object created for a {@code CallChainDirection} having a list of {@link CallChainEntry CallChainEntries}.
 */
public class CallChain {
	
	private final CallChainDirection direction;
	private List<CallChainEntry> callChainEntries;

	/**
	 * Creates a new {@link CallChain} for the given {@code direction} and an empty list of {@link CallChainEntry CallChainEntries}.
	 * 
	 * @param direction the {@link CallChainDirection}
	 */
	public CallChain(final CallChainDirection direction) {
		this(direction, new LinkedList<>());
	}

	/**
	 * Creates a new {@link CallChain} for the given {@code direction} and {@code callChain} list.
	 * 
	 * @param direction the {@link CallChainDirection}
	 * @param callChain the {@link CallChainEntry} list
	 */
	public CallChain(final CallChainDirection direction, final List<CallChainEntry> callChain) {
		this.direction = direction;
		this.callChainEntries = callChain;
	}

	/**
	 * Returns a copy of this {@link CallChain}. This method will create a new {@link CallChainEntry} list and copy the elements
	 * from this {@link CallChainEntry} list to it.
	 *
	 * @return new {@link CallChain} instance
	 */
	public CallChain copy() {
		return new CallChain(direction, new LinkedList<>(callChainEntries));
	}

	/**
	 * @return the {@link CallChainDirection} of this {@link CallChain}
	 */
	public CallChainDirection getDirection() {
		return direction;
	}
	
	/**
	 * @return the {@link CallChainEntry CallChainEntries} of this {@link CallChain}
	 */
	public List<CallChainEntry> getCallChainEntries() {
		return callChainEntries;
	}

	/**
	 * @return comma separated string of all module IDs of all {@link CallChainEntry CallChainEntries}
	 */
	public String createKey() {
		return callChainEntries.stream().map(CallChainEntry::createKey).collect(Collectors.joining(","));
	}
	
	@Override
	public int hashCode() {
		/* adapted from List.hashCode() */
		int hashCode = 1;
		for (final CallChainEntry entry : callChainEntries) {
			hashCode = 31 * hashCode + entry.module.getId().hashCode();
		}
		return Objects.hash(direction, Integer.valueOf(hashCode));
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}

		final CallChain other = (CallChain) obj;

		if (direction != other.direction) {
			return false;
		}

		if (callChainEntries.size() != other.callChainEntries.size()) {
			return false;
		}

		for (int i = 0; i < callChainEntries.size(); i++) {
			if ( ! callChainEntries.get(i).module.getId().equals(other.callChainEntries.get(i).module.getId())) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Enumeration for the {@code Call Chain} direction.
	 */
	public enum CallChainDirection {
		/** Incoming direction */
		IN,
		/** Outgoing direction */
		OUT
	}

	/**
	 * An element of a {@link CallChain}
	 */
	public static class CallChainEntry {

		private final ModuleLightweightPojo module;
		private final String callType;
		private final Map<String, Object> relationshipAttributes;

		/**
		 * Constructor
		 * 
		 * @param module the module for which this entry is created
		 * @param callType the type of reference as string like 'Calls'
		 * @param relationshipAttributes the attributes of the reference
		 */
		public CallChainEntry(final ModuleLightweightPojo module, final String callType, final Map<String, Object> relationshipAttributes) {
			this.module = module;
			this.callType = callType;
			this.relationshipAttributes = relationshipAttributes;
		}

		/**
		 * @return the {@link ModuleLightweightPojo} of this {@code Call Chain} entry
		 */
		public ModuleLightweightPojo getModule() {
			return module;
		}

		/**
		 * @return the call type of this {@code Call Chain} entry
		 */
		public String getCallType() {
			return callType;
		}

		/**
		 * @return colon separated string of module ID, call type and the relationship attributes map
		 */
		public String createKey() {
			final String accessType = getAccessType();
			return accessType == null ? module.getId().toString() + ":" + callType
									  : module.getId().toString() + ":" + callType + ":" + accessType;
		}

		/**
		 * @return the access type as string
		 */
		@Nullable
		public String getAccessType() {
			@Nullable
			var accessType = relationshipAttributes.get(ModelAttributeKey.FILE_ACCESS_TYPE.name());
			if (accessType == null) {
				accessType = relationshipAttributes.get(ModelAttributeKey.DB_ACCESS_TYPE.name());
			}
			
			return accessType == null ? null : accessType.toString();
		}
		
		/**
		 * @return the relation ship attributes of this {@code Call Chain} entry
		 */
		public Map<String, Object> getRelationshipAttributes() {
			return relationshipAttributes;
		}

		@Override
		public String toString() {
			return "CallChainEntry [module=" + module + ", callType=" + callType + ", relationshipAttributes="
					+ relationshipAttributes + "]";
		}
		
		
	}
}
