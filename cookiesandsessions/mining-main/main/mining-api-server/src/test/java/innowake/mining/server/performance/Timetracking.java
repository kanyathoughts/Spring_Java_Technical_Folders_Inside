package innowake.mining.server.performance;

import java.sql.Timestamp;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

public class Timetracking {

	private final Deque<Track> currentTrackStack = new ArrayDeque<>();
	private final List<Track> timetracks = new LinkedList<>();
	private final String initTimeStamp = new Timestamp(System.currentTimeMillis()).toString();
	private final String resultNote;
	
	public Timetracking() {
		this(null);
	}
	
	public Timetracking(final String resultNote) {
		this.resultNote = resultNote;
	}

	public void start(final String message) {
		start(message, true);
	}
	
	public void start(final String message, final boolean log) {
		final Track track = new Track(message).start();
		currentTrackStack.push(track);
		if (log) {
			timetracks.add(track);
		}
	}

	public Track stop() {
		return currentTrackStack.pop().stop();
	}
	
	public String toCompleteCsv() {
		final long samples = timetracks.size();
		long min = Long.MAX_VALUE;
		long max = Long.MIN_VALUE;
		long sum = 0;
		
		for (final Track t : timetracks) {
			final long time = t.timeNanos;
			min = Math.min(time, min);
			max = Math.max(time, max);
			sum += time;
		}
		
		final double avg = ((double) sum) / samples;
		final double err = calculateErr(timetracks, avg, samples);
		final double stddev = calculateStddev(timetracks, avg, samples);
		
		return StringUtils.joinWith(";", resultNote, initTimeStamp, Long.valueOf(samples), Long.valueOf(min), Long.valueOf(max), Double.valueOf(avg),
				Double.valueOf(err), Double.valueOf(stddev));
	}
	
	public String toReadableString() {
		final long samples = timetracks.size();
		long min = Long.MAX_VALUE;
		long max = Long.MIN_VALUE;
		long sum = 0;
		
		for (final Track t : timetracks) {
			final long time = t.timeNanos;
			min = Math.min(time, min);
			max = Math.max(time, max);
			sum += time;
		}
		
		final double avg = ((double) sum) / samples;
		final double err = calculateErr(timetracks, avg, samples);
		final double stddev = calculateStddev(timetracks, avg, samples);
		
		return StringUtils.joinWith(", ", resultNote, initTimeStamp, Long.valueOf(samples), toNanoString(min), toNanoString(max), Double.valueOf(avg),
				Double.valueOf(err), Double.valueOf(stddev));
	}
	
	private String toNanoString(final long nanos) {
		return Duration.ofNanos(nanos).toString();
	}
	
	private double calculateStddev(final List<Track> timetracks, final double avg, final long samples) {
		final double variance = timetracks.stream()
				.mapToDouble(t -> avg - t.timeNanos)
				.map(t -> t * t)
				.sum() / (samples - 1);
		
		return Math.sqrt(variance);
	}
	
	private double calculateErr(final List<Track> timetracks, final double avg, final long samples) {
		return timetracks.stream()
				.mapToDouble(t -> Math.abs(avg - t.timeNanos))
				.sum() / samples;
	}
	
	public static String csvHeader() {
		return "initTimeStamp;samples;min;max;avg;err;stddev";
	}
	
	public String toTrackCsv() {
		final String LB = System.lineSeparator();
		return timetracks.stream()
			.map(Track::toCsv)
			.collect(Collectors.joining(LB));
	}
	
	public class Track {
		private final String message;
		private long startNanos, endNanos, timeNanos;
		
		public Track(final String message) {
			this.message = message;
		}

		public Track start() {
			startNanos = System.nanoTime();
			return this;
		}
		
		public Track stop() {
			endNanos = System.nanoTime();
			timeNanos = endNanos - startNanos;
			return this;
		}
		
		public long getTime() {
			return timeNanos;
		}
		
		public String toCsv() {
			return new StringBuilder()
					.append(message)
					.append(';')
					.append(timeNanos)
					.toString();
		}
	}
}
