/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.locking;

import com.hazelcast.cp.lock.FencedLock;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.access.EntityId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.core.env.Profiles;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Service allowing to lock modules while performing certain operations.
 */
@Service
public class ModuleLockService {
	
	@Autowired
	private Environment env;

	/**
	 * Object representing a locked lock on one or several Modules.
	 * <p>
	 * <b>{@link #unlock()} must always be called!</b>
	 * <p>
	 * For convenience this class implements {@link AutoCloseable} and calling
	 * {@link #close()} is the same as calling {@link #unlock()}. It is recommended
	 * to use this class in a try-with-resources block.
	 */
	public class Lock implements AutoCloseable {
		private final List<FencedLock> locks;
		private boolean locked;

		private Lock(final List<FencedLock> locks) {
			this.locks = locks;
			locked = true;
		}

		/**
		 * Unlocks the lock.
		 */
		public void unlock() {
			locks.forEach(FencedLock::unlock);
			LOG.debug(() -> "Unlocked " + locks.stream().map(FencedLock::getName).collect(Collectors.joining(", ")));
			final Set<FencedLock> heldLocksLocal = heldLocks.get();
			locks.forEach(heldLocksLocal::remove);
			if (heldLocksLocal.isEmpty()) {
				heldLocks.remove();
			}
			locked = false;
		}

		@Override
		public void close() {
			if (locked) {
				unlock();
			}
		}
	}

	private static final Logger LOG = LoggerFactory.getLogger(ModuleLockService.class);

	private final LockManager lockManager;

	private final ThreadLocal<Set<FencedLock>> heldLocks = ThreadLocal.withInitial(HashSet::new);

	public ModuleLockService(final LockManager lockManager) {
		this.lockManager = lockManager;
	}

	/**
	 * Attempts to lock the given {@code lockName} on the given project and module
	 * and throws {@link IllegalStateException} if unable to do so within a default timeout.
	 * @param lockName the name / category of the lock to lock
	 * @param projectId the id of the project in which to create the lock
	 * @param moduleId the id of the module for which to create the lock
	 * @throws IllegalStateException if unable to acquire the lock before the timeout
	 * 
	 * @return the Lock
	 */
	public Lock tryLock(final String lockName, final Long projectId, final Long moduleId) {
		return tryLock(lockName, projectId, moduleId, 30, TimeUnit.SECONDS);
	}
	public Lock tryLock(final String lockName, final EntityId projectId, final EntityId moduleId) {
		return tryLock(lockName, projectId, moduleId, 30, TimeUnit.SECONDS);
	}

	/**
	 * Attempts to lock the given {@code lockName} on the given project and module
	 * and throws {@link IllegalStateException} if unable to do so within the given time.
	 * @param lockName the name / category of the lock to lock
	 * @param projectId the id of the project in which to create the lock
	 * @param moduleId the id of the module for which to create the lock
	 * @param time the time to wait before giving up on acquiring the lock
	 * @param unit the time unit for the {@code time} parameter
	 * @throws IllegalStateException if unable to acquire the lock before the timeout
	 *
	 * @return the Lock
	 */
	@SuppressWarnings("java:S2222") /* the lock is returned to the caller, so it can't be unlocked here */
	public Lock tryLock(final String lockName, final Long projectId, final Long moduleId,
							   final long time, final TimeUnit unit) {
		final String lockNameWithIds = lockName + "." + projectId + "." + moduleId;
		final FencedLock fencedLock = lockManager.getLock(lockNameWithIds);
		final boolean locked = fencedLock.tryLock(time, unit);
		if ( ! locked) {
			LOG.error("ModuleLockService: Failed to acquire lock for " + fencedLock.getName() +  ", held locks: " + heldLocks.get());
			throw new IllegalStateException("ModuleLockService: Failed to acquire lock for " + fencedLock.getName());
		}
		LOG.debug(() -> "Locked " + fencedLock.getName());
		heldLocks.get().add(fencedLock);
		return new Lock(Collections.singletonList(fencedLock));
	}

	@SuppressWarnings("java:S2222") /* the lock is returned to the caller, so it can't be unlocked here */
	public Lock tryLock(final String lockName, final EntityId projectId, final EntityId moduleId,
						final long time, final TimeUnit unit) {
		final String lockNameWithIds = lockName + "." + projectId.getNidOptional().map(Object::toString).orElseGet(() -> projectId.getUid().toString()) + "." + moduleId.getNidOptional().map(Object::toString).orElseGet(() -> moduleId.getUid().toString());
		final FencedLock fencedLock = lockManager.getLock(lockNameWithIds);
		final boolean locked = fencedLock.tryLock(time, unit);
		if ( ! locked) {
			LOG.error("ModuleLockService: Failed to acquire lock for " + fencedLock.getName() +  ", held locks: " + heldLocks.get());
			throw new IllegalStateException("ModuleLockService: Failed to acquire lock for " + fencedLock.getName());
		}
		LOG.debug(() -> "Locked " + fencedLock.getName());
		heldLocks.get().add(fencedLock);
		return new Lock(Collections.singletonList(fencedLock));
	}

	/**
	 * Attempts to lock the given {@code lockName} on the given project and list of modules. The modules are locked in
	 * deterministic order to avoid deadlocking when multiple Threads need to acquire a lock on the same module.
	 * Throws {@link IllegalStateException} if unable to acquire the lock after a default timeout.
	 * @param lockName the name / category of the lock to lock
	 * @param projectId the id of the project in which to create the lock
	 * @param moduleIds list of module ids for which to create the lock
	 * @throws IllegalStateException if unable to acquire the lock before the timeout
	 * 
	 * @return the Lock
	 */
	public Lock tryLock(final String lockName, final Long projectId, final List<Long> moduleIds) {
		return tryLock(lockName, projectId, moduleIds, 30, TimeUnit.SECONDS);
	}

	/**
	 * Attempts to lock the given {@code lockName} on the given project and list of modules. The modules are locked in
	 * deterministic order to avoid deadlocking when multiple Threads need to acquire a lock on the same module.
	 * Throws {@link IllegalStateException} if unable to acquire the lock within the given time.
	 * @param lockName the name / category of the lock to lock
	 * @param projectId the id of the project in which to create the lock
	 * @param moduleIds list of module ids for which to create the lock
	 * @param time the maximum time to wait for the lock
	 * @param unit the time unit of the time argument
	 * @throws IllegalStateException if unable to acquire the lock before the timeout
	 * 
	 * @return the Lock
	 */
	public Lock tryLock(final String lockName, final Long projectId, final List<Long> moduleIds,
						long time, TimeUnit unit) {

		if (env.acceptsProfiles(Profiles.of("test"))) {
			time = 1;
			unit = TimeUnit.DAYS;
		}
		final List<FencedLock> lockedLocks = new ArrayList<>(moduleIds.size());
		final List<Long> moduleIdsSorted = moduleIds.stream().sorted().toList();
		boolean failed = false;
		String failedLockName = "";
		for (final Long moduleId : moduleIdsSorted) {
			final FencedLock fencedLock = lockManager.getLock(lockName + "." + projectId + "." + moduleId);
			@SuppressWarnings("java:S2222") /* We add the lock to heldLocks and unlock them when the returned lock is closed */
			final boolean locked = fencedLock.tryLock(time, unit);
			if (locked) {
				lockedLocks.add(fencedLock);
			} else {
				failed = true;
				failedLockName = fencedLock.getName();
				break;
			}
		}
		if (failed) {
			lockedLocks.forEach(FencedLock::unlock);
			LOG.error("Failed to acquire lock for " + failedLockName +  ", held locks: " + heldLocks.get());
			throw new IllegalStateException("Failed to acquire lock for " + failedLockName);
		}
		LOG.debug(() -> "Locked " + lockedLocks.stream().map(FencedLock::getName).collect(Collectors.joining(", ")));
		heldLocks.get().addAll(lockedLocks);
		return new Lock(lockedLocks);
	}
}
