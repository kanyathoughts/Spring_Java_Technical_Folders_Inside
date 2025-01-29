/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Objects;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;

/**
 * Set of elements to be added to or removed from some entity.
 * @param <T> Type of elements maintained by this set.
 */
public class DiffSet<T extends Serializable> implements Set<T>, Serializable {

	private final Set<T> additions;
	private final Set<T> deletions;
	
	public DiffSet() {
		this.additions = new HashSet<>();
		this.deletions = new HashSet<>();
	}
	
	public DiffSet(final Set<T> additions, final Set<T> deletions) {
		this.additions = additions;
		this.deletions = deletions;
	}

	@Override
	public int size() {
		return additions.size() + deletions.size();
	}

	@Override
	public boolean isEmpty() {
		return additions.isEmpty() && deletions.isEmpty();
	}

	@Override
	public boolean contains(@Nullable final Object o) {
		return additions.contains(o) || deletions.contains(o);
	}

	@Override
	public Iterator<T> iterator() {
		return additions.iterator();
	}

	@Override
	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}

	@Override
	public <A> A[] toArray(@Nullable A[] a) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean add(@Nullable final T e) {
		return put(Objects.requireNonNull(e), true);
	}

	/**
	 * Defines an element to be either added or deleted.
	 * @param e Element value.
	 * @param addition If it is to be added ({@code true}) or deleted ({@code false}).
	 * @return If any definition changed.
	 */
	public boolean put(final T e, final boolean addition) {
		if (addition) {
			final boolean x = this.deletions.remove(e);
			return this.additions.add(e) || x;
		} else {
			final boolean x = this.additions.remove(e);
			return this.deletions.add(e) || x;
		}
	}
	
	/**
	 * Defines an element to be added. 
	 * @param e Element value.
	 * @return this
	 */
	public DiffSet<T> addition(final T e) {
		put(e, true);
		return this;
	}
	
	/**
	 * Defines an element to be deleted. 
	 * @param e Element value.
	 * @return this
	 */
	public DiffSet<T> deletion(final T e) {
		put(e, false);
		return this;
	}
	
	public Set<T> getAdditions() {
		return this.additions;
	}
	
	public Set<T> getDeletions() {
		return this.deletions;
	}

	@Override
	public boolean remove(@Nullable final Object o) {
		final boolean x = this.additions.remove(o);
		return this.deletions.remove(o) || x;
	}

	@Override
	public boolean containsAll(@Nullable final Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean addAll(@Nullable final Collection<? extends T> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean retainAll(@Nullable final Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean removeAll(@Nullable final Collection<?> c) {
		final boolean x = additions.removeAll(c);
		return deletions.removeAll(c) || x;
	}

	@Override
	public void clear() {
		additions.clear();
		deletions.clear();
	}

	@Override
	public String toString() {
		return "[additions:" + additions.toString() + ",deletions:" + deletions.toString() + "]";
	}

}
