package innowake.mining.shared.entities.functionalblocks;

import java.util.List;

/**
 * Results object for a functional block.
 */
public class FunctionalBlockWithChildrenAndLinks {

	private final FunctionalBlockPojoPrototype functionalBlock;
	private final List<FunctionalBlockPojoPrototype> children;
	private final List<FunctionalBlockLink> functionalBlockLinks;

	/**
	 * Constructor with parameters.
	 * @param functionalBlock the functional block
	 * @param children the list of functional block prototypes
	 * @param functionalBlockLinks the list of functional block links
	 */
	public FunctionalBlockWithChildrenAndLinks(final FunctionalBlockPojoPrototype functionalBlock, final List<FunctionalBlockPojoPrototype> children,
			final List<FunctionalBlockLink> functionalBlockLinks) {
		this.functionalBlock = functionalBlock;
		this.children = children;
		this.functionalBlockLinks = functionalBlockLinks;
	}

	/**
	 * Returns the functional block.
	 * @return the functional block
	 */
	public FunctionalBlockPojoPrototype getFunctionalBlock() {
		return functionalBlock;
	}

	/**
	 * Returns the list of functional block prototypes.
	 * @return the list of functional block prototypes
	 */
	public List<FunctionalBlockPojoPrototype> getChildren() {
		return children;
	}

	/**
	 * Returns the list of functional block links.
	 * @return the list of functional block links
	 */
	public List<FunctionalBlockLink> getFunctionalBlockLinks() {
		return functionalBlockLinks;
	}
}
