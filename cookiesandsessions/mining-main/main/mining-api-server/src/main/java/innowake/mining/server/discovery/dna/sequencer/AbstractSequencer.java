/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.io.discovery.config.Configurable;
import innowake.mining.server.discovery.parser.ParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.core.parsing.ILocationMultiline;

/**
 * Base class for sequencers. This class loads the configuration and defines common config properties for all sequencers.
 * @param <T> the type of the parse result that this sequencer can handle
 */
public abstract class AbstractSequencer<T> extends Configurable implements Sequencer {
	
	protected final ParseResultProvider<T> parser;

	/**
	 * Initializes the AbstractSequencer, loading its configuration form a file named {@code fileName} from the given {@code project}. 
	 * 
	 * @param configSupplier Supplier of the configuration
	 * @param parser the parse result provide used to parse source objects
	 */
	protected AbstractSequencer(final Supplier<String> configSupplier, final ParseResultProvider<T> parser) {
		super(configSupplier);
		this.parser = parser;
		loadAndApplyConfiguration();
	}
	
	protected abstract Collection<SequencerRule<T>> getRules();
	
	@Override
	public List<Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>>> apply(final SourcePojo sourceObject) throws DiscoveryException {
		final List<Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>>> result = new ArrayList<>();
		final DNACollector<T> collector = new DNACollector<>(sourceObject, parser);
		for (final SequencerRule<T> rule : getRules()) {
			final DnaSequencer id = rule.getId();
			collector.reset();
			rule.apply(collector);
			final List<DNAItem> dnaItems = collector.getDNAItems();
			final DnaStringPojoPrototype dnaString = new DnaStringPojoPrototype();
			dnaString.setContentHash(sourceObject.getContentHash().get());
			dnaString.setSequencer(id);
			final List<DnaStringElementPojoPrototype> dnaStringElements = new ArrayList<>(dnaItems.size());
			int index = 0;
			for (final DNAItem dnaItem : dnaItems) {
				final var dnaStringElement = new DnaStringElementPojoPrototype();
				final Optional<ILocationMultiline> location = dnaItem.getLocation();
				if (location.isPresent()) {
					final ILocationMultiline iLocationMultiline = location.get();
					dnaStringElement.setLocation(new ModuleLocation(iLocationMultiline.getOffset(), iLocationMultiline.getLength()));
				}
				dnaStringElement.setValue(dnaItem.getValue());
				dnaStringElement.setIndex(index);
				dnaStringElements.add(dnaStringElement);
				index++;
			}

			result.add(new Tuple2<>(dnaString, dnaStringElements));
		}
		return Collections.unmodifiableList(result);
	}
}
