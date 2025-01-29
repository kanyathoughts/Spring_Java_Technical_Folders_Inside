package innowake.mining.server.discovery.dna.community.louvain;

import java.util.ArrayList;
import java.util.List;

/**
 * Contains graphs for unit tests.
 */
public final class TestGraphFactory {
	
	private TestGraphFactory() {}
	
	/**
	 * A graph containing two triangles which are connected by two edges.
	 *
	 * @return The test graph
	 */
	public static UnitTestGraph createSimple2CliquesGraph() {
		final List<Node> nodes = new ArrayList<>();
		final Node n0_1 = new Node();
		final Node n0_2 = new Node();
		final Node n0_3 = new Node();
		final Node n1_1 = new Node();
		final Node n1_2 = new Node();
		final Node n1_3 = new Node();
		n0_1.addAdjacentNode(n0_2, 1);
		n0_1.addAdjacentNode(n0_3, 1);
		n0_2.addAdjacentNode(n0_3, 1);
		
		n0_1.addAdjacentNode(n1_2, 1);
		n0_2.addAdjacentNode(n1_1, 1);
		
		n1_1.addAdjacentNode(n1_2, 1);
		n1_1.addAdjacentNode(n1_3, 1);
		n1_2.addAdjacentNode(n1_3, 1);
		
		nodes.add(n0_1);
		nodes.add(n0_2);
		nodes.add(n0_3);
		nodes.add(n1_1);
		nodes.add(n1_2);
		nodes.add(n1_3);
	
		return new UnitTestGraph(nodes);
	}
	

	/**
	 * This graph has been used alot in scientific papers to this topic so it should be well suited for unit tests.
	 * @see <a href="https://arxiv.org/pdf/0803.0476.pdf">Fast unfolding of communities in large networks</a>
	 * @return The test graph
	 */
	public static UnitTestGraph create30CliquesGraph() {
		final List<Node> nodes = new ArrayList<>();
		/* Create 30 cliques. */
		for(int i=0; i<30;i++) {
			final Node n1 = new Node();
			final Node n2 = new Node();
			final Node n3 = new Node();
			final Node n4 = new Node();
			final Node n5 = new Node();
			n1.addAdjacentNode(n2, 1);
			n2.addAdjacentNode(n3, 1);
			n3.addAdjacentNode(n4, 1);
			n4.addAdjacentNode(n5, 1);
			n5.addAdjacentNode(n1, 1);
			n1.addAdjacentNode(n3, 1);
			n1.addAdjacentNode(n4, 1);
			n2.addAdjacentNode(n4, 1);
			n2.addAdjacentNode(n5, 1);
			n3.addAdjacentNode(n5, 1);
			nodes.add(n1);
			nodes.add(n2);
			nodes.add(n3);
			nodes.add(n4);
			nodes.add(n5);
		}

		/* Connect all cliques. */
		for (int i = 5; i < 30 * 5; i += 5) {
			final Node n1_1 = nodes.get(i - 3);
			final Node n2_1 = nodes.get(i);
			n1_1.addAdjacentNode(n2_1, 1);
		}
		
		/* Close the circle */
		nodes.get(0).addAdjacentNode(nodes.get(147), 1);
		
		return new UnitTestGraph(nodes);
	}
}
