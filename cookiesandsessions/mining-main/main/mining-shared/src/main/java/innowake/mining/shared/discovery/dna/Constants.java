/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.dna;

/**
 * Basic Constants
 */
public interface Constants {

	/**
	 * Separator char used to separate values in dna csv files.
	 */
	public static final String SEPARATOR = ";";
	
	/**
	 * Filename for the dna string file.
	 */
	public static final String DNA_STRING_FILENAME = "DNAStrings.csv";
	
	/**
	 * Filename for the similarity file.
	 */
	public static final String DNA_SIMILARITY_FILENAME = "DNASimilarities.csv";
	
	/**
	 * Filename for the community file.
	 */
	public static final String DNA_COMMUNITY_FILENAME = "DNACommunities.csv";
	
	/**
	 * Folder name for all DNA generated files.
	 */
	public static final String DNA_BASE_FOLDER = "DNA";
	
}
