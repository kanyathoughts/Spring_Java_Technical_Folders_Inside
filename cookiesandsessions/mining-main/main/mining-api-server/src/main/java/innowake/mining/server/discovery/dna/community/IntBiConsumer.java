/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.community;

/**
 * Primitive int consumer for two arguments
 */
public interface IntBiConsumer {

	/**
	 * Consume two int arguments
	 *
	 * @param p1 Argument one
	 * @param p2 Argument two
	 */
	void accept(int p1, int p2);
}
