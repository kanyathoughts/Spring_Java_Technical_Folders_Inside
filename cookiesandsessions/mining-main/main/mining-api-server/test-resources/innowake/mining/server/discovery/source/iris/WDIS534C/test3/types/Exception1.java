/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534C.test3.types;

public class Exception1 extends RuntimeException {

	public Exception1(final Exception exc) {
		super(exc);
	}
}
