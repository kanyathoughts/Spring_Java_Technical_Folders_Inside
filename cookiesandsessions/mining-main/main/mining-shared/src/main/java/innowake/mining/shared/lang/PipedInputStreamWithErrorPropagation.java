/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.concurrent.atomic.AtomicReference;

import innowake.lib.core.api.lang.Nullable;

/**
 * A {@link PipedInputStream} that allows to propagate exceptions from the producer to the consumer.
 */
public class PipedInputStreamWithErrorPropagation extends PipedInputStream {

	private final AtomicReference<Throwable> exception = new AtomicReference<>();

	/**
	 * Creates a <code>PipedInputStream</code> so
	 * that it is not yet {@linkplain #connect(java.io.PipedOutputStream)
	 * connected}.
	 * It must be {@linkplain java.io.PipedOutputStream#connect(
	 * java.io.PipedInputStream) connected} to a
	 * <code>PipedOutputStream</code> before being used.
	 */
	public PipedInputStreamWithErrorPropagation() {
		super();
	}

	/**
	 * Creates a <code>PipedInputStream</code> so that it is not yet
	 * {@linkplain #connect(java.io.PipedOutputStream) connected} and
	 * uses the specified pipe size for the pipe's buffer.
	 * It must be {@linkplain java.io.PipedOutputStream#connect(
	 * java.io.PipedInputStream)
	 * connected} to a <code>PipedOutputStream</code> before being used.
	 *
	 * @param      pipeSize the size of the pipe's buffer.
	 * @exception  IllegalArgumentException if {@code pipeSize <= 0}.
	 * @since      1.6
	 */
	public PipedInputStreamWithErrorPropagation(final int pipeSize) {
		super(pipeSize);
	}

	/**
	 * Creates a <code>PipedInputStream</code> so that it is
	 * connected to the piped output stream
	 * <code>src</code> and uses the specified pipe size for
	 * the pipe's buffer.
	 * Data bytes written to <code>src</code> will then
	 * be available as input from this stream.
	 *
	 * @param      src   the stream to connect to.
	 * @param      pipeSize the size of the pipe's buffer.
	 * @exception  IOException  if an I/O error occurs.
	 * @exception  IllegalArgumentException if {@code pipeSize <= 0}.
	 * @since      1.6
	 */
	public PipedInputStreamWithErrorPropagation(final PipedOutputStream src, final int pipeSize) throws IOException {
		super(src, pipeSize);
	}

	/**
	 * Creates a <code>PipedInputStream</code> so
	 * that it is connected to the piped output
	 * stream <code>src</code>. Data bytes written
	 * to <code>src</code> will then be  available
	 * as input from this stream.
	 *
	 * @param      src   the stream to connect to.
	 * @exception  IOException  if an I/O error occurs.
	 */
	public PipedInputStreamWithErrorPropagation(final PipedOutputStream src) throws IOException {
		super(src);
	}

	@Override
	public synchronized int read() throws IOException {
		final int result = super.read();
		final Throwable storedException = exception.get();
		if (storedException != null) {
			throw new IOException("Exception in producer Thread", storedException);
		}
		return result;
	}

	@Override
	public synchronized int read(@Nullable final byte[] b, final int off, final int len) throws IOException {
		final int result = super.read(b, off, len);
		return result;
	}

	/**
	 * Sets a throwable that originated in the producer thread and that shall be thrown in the consumer thread.
	 * <p>
	 * The next read operation on this stream will throw an {@link IOException} with the given {@code Throwable} as cause.
	 *
	 * @param cause the cause for failure
	 */
	public void failWith(final Throwable cause) {
		exception.set(cause);
	}
}
