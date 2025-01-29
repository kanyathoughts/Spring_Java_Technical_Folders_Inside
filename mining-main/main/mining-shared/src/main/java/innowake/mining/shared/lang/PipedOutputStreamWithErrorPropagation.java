/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

import innowake.lib.core.api.lang.Nullable;

/**
 * A {@link PipedOutputStream} that allows to propagate exceptions from the producer to the consumer.
 */
public class PipedOutputStreamWithErrorPropagation extends PipedOutputStream {

	@Nullable
	private PipedInputStream sink;

	/**
	 * Creates a piped output stream that is not yet connected to a
	 * piped input stream. It must be connected to a piped input stream,
	 * either by the receiver or the sender, before being used.
	 *
	 * @see     java.io.PipedInputStream#connect(java.io.PipedOutputStream)
	 * @see     java.io.PipedOutputStream#connect(java.io.PipedInputStream)
	 */
	public PipedOutputStreamWithErrorPropagation() {
		super();
	}

	/**
	 * Creates a piped output stream connected to the specified piped
	 * input stream. Data bytes written to this stream will then be
	 * available as input from <code>snk</code>.
	 *
	 * @param      snk   The piped input stream to connect to.
	 * @exception  IOException  if an I/O error occurs.
	 */
	public PipedOutputStreamWithErrorPropagation(final PipedInputStream snk) throws IOException {
		super(snk);
	}

	@Override
	public synchronized void connect(@Nullable final PipedInputStream snk) throws IOException {
		sink = snk;
		super.connect(snk);
	}

	/**
	 * Sets a throwable that originated in the producer thread and that shall be thrown in the consumer thread.
	 * <p>
	 * The next read operation on the connected piped input stream will throw an {@link IOException} with the given {@code Throwable} as cause.
	 *
	 * @param cause the cause for failure
	 */
	public void failWith(final Throwable cause) {
		if (sink instanceof PipedInputStreamWithErrorPropagation) {
			((PipedInputStreamWithErrorPropagation) sink).failWith(cause);
		}
	}
}
