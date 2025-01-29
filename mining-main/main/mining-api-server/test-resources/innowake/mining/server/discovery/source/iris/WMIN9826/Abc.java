class Abc {
	ImmutableSet<Entry<K, V>> createEntrySet() {
		class EntrySetImpl extends ImmutableMapEntrySet<K, V> {

			@Override
			ImmutableMap<K, V> map() {
				return IteratorBasedImmutableMap.this;
			}

			@Override
			public UnmodifiableIterator<Entry<K, V>> iterator() {
				return entryIterator();
			}
		}
		return new EntrySetImpl();
	}
}
