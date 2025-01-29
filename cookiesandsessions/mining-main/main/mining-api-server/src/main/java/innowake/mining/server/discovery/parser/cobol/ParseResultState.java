/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.cobol;

/**
 * Enumeration to keep the state of a parse process.
 */
public enum ParseResultState {
	
	/**
	 * Parsing was successful
	 */
	SUCCESS, 
	
	/**
	 * Parsing failed and result is maybe not complete.
	 */
	ERROR
}
