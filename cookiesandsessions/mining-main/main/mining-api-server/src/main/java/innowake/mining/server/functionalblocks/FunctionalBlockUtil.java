package innowake.mining.server.functionalblocks;

import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;

/**
 * Utility class for the {@link FunctionalBlockPojo}.
 */
public class FunctionalBlockUtil {

	private FunctionalBlockUtil() {
	}

	/**
	 * Returns whether the given functional block has the given type (i.e. whether the {@linkplain FunctionalBlockFlag#TYPE TYPE} flag contains
	 * the given type).
	 * @param functionalBlock the functional block
	 * @param type the type to check
	 * @return {@code true} if the functional block has the type, else {@code false}
	 */
	public static boolean hasType(final FunctionalBlockPojo functionalBlock, final FunctionalBlockType type) {
		final Object o = functionalBlock.getFlags().get(FunctionalBlockFlag.TYPE.name());
		if ( o == null ) {
			return false;
		}
		if ( o instanceof Collection ) {
			return ( (Collection<?>) o ).contains(type.toString());
		}
		return o.toString().equals(type.toString());
	}

	/**
	 * Returns whether the given functional block has all of the given types (i.e. whether the {@linkplain FunctionalBlockFlag#TYPE TYPE} flag contains
	 * any of the given types).
	 * @param functionalBlock the functional block
	 * @param blockTypes the types to check
	 * @return {@code true} if the functional block has all of the given types, else {@code false}
	 */
	public static boolean hasType(final FunctionalBlockPojo functionalBlock, final Collection<FunctionalBlockType> blockTypes) {
		final Object o = functionalBlock.getFlags().get(FunctionalBlockFlag.TYPE.name());
		if ( o == null ) {
			return false;
		}
		if ( o instanceof Collection ) {
			return ( (Collection<String>) o ).containsAll(blockTypes.stream().map(FunctionalBlockType :: name).toList());
		}
		return false;
	}

	/**
	 * Returns whether the given functional block link has the given type (i.e. whether the {@linkplain FunctionalBlockLinkFlag#TYPE TYPE} flag contains
	 * the given type).
	 * @param link the functional block link
	 * @param type the type to check
	 * @return {@code true} if the functional block link has the type, else {@code false}
	 */
	public static boolean hasType(final FunctionalBlockLink link, final FunctionalBlockLinkType type) {
		final Object o = link.getFlags().get(FunctionalBlockLinkFlag.TYPE.name());
		if ( o == null ) {
			return false;
		}
		if ( o instanceof Collection ) {
			return ( (Collection<?>) o ).contains(type.toString());
		}
		return o.toString().equals(type.toString());
	}

	public static Optional<String> getStringFlag(final Map<String, Object> flags, final FunctionalBlockFlag flag) {
		final Object o = flags.get(flag.name());
		if ( o == null ) {
			return Optional.empty();
		}
		return Optional.of(o.toString());
	}
}
