/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package wdis534c.test3.types;

public class Exception1 extends RuntimeException {

	public Exception1(final Exception exc) {
		super(exc);
	}
}
