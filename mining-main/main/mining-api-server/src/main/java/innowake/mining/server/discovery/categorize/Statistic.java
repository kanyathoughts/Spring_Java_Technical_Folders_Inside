/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize;

import static java.lang.Integer.valueOf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.discovery.Logging;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.config.core.XMLResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.job.Message;

/**
 * Accumulates information about the detection
 */
public class Statistic {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CATEGORIZE_STATISTIC);
	
	private final Map<ResolveTarget, AtomicInteger> discovered = new ConcurrentHashMap<>();
	private final Map<ResolveTarget, AtomicInteger> duplicates = new ConcurrentHashMap<>();
	private final Map<ResolveTarget, AtomicInteger> maybe = new ConcurrentHashMap<>();
	private int selected;
	private int unidentified;

	/**
	 * Clears any recorded information in this instance.
	 */
	public void clear() {
		selected = 0;
		unidentified = 0;
		discovered.clear();
		duplicates.clear();
	}

	/**
	 * Sets the size of selected files.
	 *
	 * @param selected the number of selected files
	 */
	public void setSelected(final int selected) {
		this.selected = selected;
	}

	/**
	 * Sets the size of unidentified files.
	 *
	 * @param unidentified the number of unidentified files
	 */
	public void setUnidentified(final int unidentified) {
		this.unidentified = unidentified;
	}
	
	/**
	 * Adds a discovered resource
	 *
	 * @param identification the discovered resource
	 */
	public void addDiscovered(final Identification identification) {
		discovered.computeIfAbsent(identification.getTarget(), key -> new AtomicInteger()).incrementAndGet();
	}

	/**
	 * Add a resource to the maybe identified pool.
	 *
	 * @param identification of the discovered resource.
	 */
	public void addMaybe(final Identification identification) {
		maybe.computeIfAbsent(identification.getTarget(), key -> new AtomicInteger()).incrementAndGet();
	}
	
	/**
	 * Adds a duplicate resource
	 *
	 * @param identification the duplicate resource
	 */
	public void addDuplicate(final Identification identification) {
		duplicates.computeIfAbsent(identification.getTarget(), key -> new AtomicInteger()).incrementAndGet();
	}

	/**
	 * Print statistic summary.
	 *
	 * @param jobMonitor the job monitor
	 */
	public void printSummary(final JobMonitor jobMonitor) {
		final List<String> log = new ArrayList<>();
		final List<String> jobMonitorMessage = new ArrayList<>();
		final String overallStatistic = String.format("Overall selected objects: %s", valueOf(selected));
		
		log.add("-------------------------------------");
		log.add("Finished setting names and extensions");
		log.add(overallStatistic);
		jobMonitorMessage.add(overallStatistic);
		
		Arrays.stream(ResolveTarget.values())
			.filter(discovered::containsKey)
			.map(target -> String.format("Identified %s(s): %s", new XMLResolveTarget(target).getNameFormatted(), valueOf(discovered.get(target).get())))
			.forEach(message -> {
				log.add(message);
				jobMonitorMessage.add(message);
			});
		
		Arrays.stream(ResolveTarget.values())
			.filter(maybe::containsKey)
			.map(target -> String.format("Identified maybe as %s(s): %s", new XMLResolveTarget(target).getNameFormatted(), valueOf(maybe.get(target).get())))
			.forEach(message -> {
				log.add(message);
				jobMonitorMessage.add(message);
			});
		
		Arrays.stream(ResolveTarget.values())
			.filter(duplicates::containsKey)
			.map(target -> target == ResolveTarget.NONE ?
					"Unidentified file name clash: " + valueOf(duplicates.get(target).get()):
					String.format("%s name clash: %s", new XMLResolveTarget(target).getNameFormatted(), valueOf(duplicates.get(target).get()))
			).forEach(message -> {
				log.add(message);
				jobMonitorMessage.add(message);
			});
		
		final String unidentifiedStatistic = "Unidentified objects: " + valueOf(unidentified);
		log.add(unidentifiedStatistic);
		jobMonitorMessage.add(unidentifiedStatistic);
		log.add("-------------------------------------");
		if (LOG.isInfoEnabled()) {
			log.forEach(LOG::info);
		}
		jobMonitorMessage.forEach(message -> jobMonitor.addMessage(new Message(Message.Severity.INFO, message)));
	}

}
