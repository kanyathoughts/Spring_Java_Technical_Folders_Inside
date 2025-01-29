/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.locking;

import com.hazelcast.core.EntryEvent;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.cp.lock.FencedLock;
import com.hazelcast.map.IMap;
import com.hazelcast.map.listener.EntryRemovedListener;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.security.AuthenticationFacade;
import innowake.mining.server.service.UserNameService;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import org.javatuples.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.Serializable;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Service for acquiring a lock of a certain {@linkplain ProjectLockCategory category} on a project.
 */
@Service
@Scope(value = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class ProjectLockService {
	
	/**
	 * Object representing a lock on a Project. The owner of this object <b>must</b> call the {@link #unlock()} or {@link #close()} method
	 * to release the lock.
	 */
	public class ProjectLock implements AutoCloseable {

		private final EntityId projectId;
		private final ProjectLockCategory lockCategory;
		private final FencedLock lock;

		public ProjectLock(final EntityId projectId, final ProjectLockCategory lockCategory, final FencedLock lock) {
			this.projectId = projectId;
			this.lockCategory = lockCategory;
			this.lock = lock;
		}

		/**
		 * Releases the project lock.
		 */
		public void unlock() {
			/* same thread can acquire the lock multiple times (reentrant lock): ownership information is only cleared when last lock is released */
			final int reentrantLockCount = lock.getLockCount();
			if (reentrantLockCount == 1) {
				/* last reentrant lock is about to be unlocked */
				Optional.ofNullable(ownershipInfoMap).orElseThrow(() -> new IllegalStateException(OWNERSHIP_INFO_MAP_NULL_MESSAGE))
						.remove(new Pair<>(projectId, lockCategory));
			}
			lock.unlock();
		}

		/**
		 * Same as {@link #unlock()}. Allows to use this class in try-with-resources statement.
		 */
		@Override
		public void close() {
			unlock();
		}
	}

	/**
	 * Thrown from {@link ProjectLockService#tryLock(EntityId, ProjectLockCategory, String)} in case a lock could not be acquired within the default
	 * timeout for interactive operations. Contains detailed information about who currently holds the lock that can be shown to the user.
	 */
	public static class AlreadyLockedException extends Exception {
		private final EntityId projectId;
		private final ProjectLockCategory lockCategory;
		private final ProjectLockOwnershipInfo ownershipInfo;

		public AlreadyLockedException(final EntityId projectId, final ProjectLockCategory lockCategory, final ProjectLockOwnershipInfo ownershipInfo) {
			super("Failed to acquire lock " + lockCategory + " on project " + projectId + ": lock is already held");
			this.projectId = projectId;
			this.lockCategory = lockCategory;
			this.ownershipInfo = ownershipInfo;
		}

		public EntityId getProjectId() {
			return projectId;
		}

		public ProjectLockCategory getLockCategory() {
			return lockCategory;
		}

		public ProjectLockOwnershipInfo getOwnershipInfo() {
			return ownershipInfo;
		}
	}

	/**
	 * Provides detailed information about who is currently holding a lock.
	 */
	public static class ProjectLockOwnershipInfo implements Serializable {
		private final String owningUserId;
		private final String owningUserName;
		private final String reasonPhrase;

		public ProjectLockOwnershipInfo(final String owningUserId, final String owningUserName, final String reasonPhrase) {
			this.owningUserId = owningUserId;
			this.owningUserName = owningUserName;
			this.reasonPhrase = reasonPhrase;
		}

		public String getOwningUserId() {
			return owningUserId;
		}

		public String getOwningUserName() {
			return owningUserName;
		}

		public String getReasonPhrase() {
			return reasonPhrase;
		}
	}

	/* wait 5 seconds for a lock before telling the user that the operation is currently not possible */
	private static final int DEFAULT_LOCK_TIMEOUT_SECONDS = 5;
	private static final String OWNERSHIP_INFO_MAP_NULL_MESSAGE = "ownershipInfoMap is null";

	@Autowired
	private LockManager lockManager;

	@Autowired
	private HazelcastInstance hz;

	@Autowired
	private AuthenticationFacade authenticationFacade;

	@Autowired
	private UserNameService userNameService;

	// using the Pair implementation from org.javatuples here because it is Serializable
	// everything stored in a Hazelcast IMap must be serializable
	@Nullable
	private IMap<Pair<EntityId, ProjectLockCategory>, ProjectLockOwnershipInfo> ownershipInfoMap;

	@PostConstruct
	public void init() {
		// name doesn't matter but needs to be unique
		ownershipInfoMap = hz.getMap("ProjectLockService_ownershipInfoMap");
	}

	/**
	 * Try to acquire a lock with the given category on the given project. If the lock is already locked, this method will wait for a short time for the
	 * lock to become available and otherwise throw {@link AlreadyLockedException}. The exception will contain detailed information about who is currently
	 * holding the lock.
	 * <p>
	 * This is the appropriate method to use when you need to require a lock for an interactive user action, for example when a user tries to modify
	 * Taxonomies through mining-ui. The user operation will wait for a short time for the lock to become available and otherwise report an error message
	 * back to the user indicating that they need to retry the operation later.
	 * </p>
	 * <p>
	 * <b>Important</b>: the caller is responsible to unlock the returned {@link ProjectLock}. To facilitate this, {@code ProjectLock} implements 
	 * {@link AutoCloseable}, so it is recommeded to use it in a try-with-resources block. Failure to unlock the returned lock will lead to deadlocks.
	 * </p>
	 *
	 * @param projectId id of the project on which to acquire the lock
	 * @param lockCategory the category of the lock
	 * @param reasonPhrase a reason phrase explaining why the lock was acquired, e.g. "Deleting Taxonomy"
	 * @return a project lock; the returned lock <b>must</b> be unlocked by the caller
	 * @throws AlreadyLockedException if the lock is already locked and could not be acquired within the timeout
	 */
	public ProjectLock tryLock(final EntityId projectId, final ProjectLockCategory lockCategory, final String reasonPhrase) throws AlreadyLockedException {
		final String userId = authenticationFacade.getUserId();
		final String userName = lookupUserName(userId);
		final ProjectLockOwnershipInfo ownershipInfo = new ProjectLockOwnershipInfo(userId, userName, reasonPhrase);
		final Optional<ProjectLock> projectLock = tryLockAndUpdateOwnership(projectId, lockCategory, ownershipInfo);
		if (projectLock.isPresent()) {
			return projectLock.get();
		} else {
			final ProjectLockOwnershipInfo existingOwnership = Optional.ofNullable(ownershipInfoMap)
					.orElseThrow(() -> new IllegalStateException(OWNERSHIP_INFO_MAP_NULL_MESSAGE)).get(new Pair<>(projectId, lockCategory));
			throw new AlreadyLockedException(projectId, lockCategory, existingOwnership);
		}
	}

	/**
	 * Wait until the requested lock is available and then pass the acquired {@link ProjectLock} to the given lock consumer.
	 * If the lock is available immediately, the lock consumer will be called from the current Thread. Otherwise the lock consumer will be called
	 * asynchronously from a background Thread once the lock becomes available.
	 * <p>
	 * This is the appropriate method to use when you need to acquire a lock for a scheduled, long-running background Task, like a Job.
	 * The specified callback will be invoked as soon as the lock becomes available and allows you to execute the background action
	 * asynchronously with the lock held.
	 * </p>
	 * <p>
	 * <b>Important</b>: the lock consumer is responsible to unlock the returned {@link ProjectLock}. Failure to unlock the lock will lead to deadlocks.
	 * </p>
	 * @param projectId id of the project on which to acquire the lock
	 * @param lockCategory the category of the lock
	 * @param reasonPhrase a reason phrase explaining why the lock was acquired, e.g. "Executing Taxonomy Bulk Assignment"
	 * @param lockConsumer a callback that will be called once the lock is available; acquired lock is passed to the callback and must be unlocked by it
	 */
	public void waitForLockAndRun(final EntityId projectId, final ProjectLockCategory lockCategory, final String reasonPhrase,
			final Consumer<ProjectLock> lockConsumer) {
		final AtomicBoolean callbackCalled = new AtomicBoolean(false);
		final AtomicReference<UUID> listenerIdRef = new AtomicReference<>(null);
		
		final UUID listenerId = Optional.ofNullable(ownershipInfoMap).orElseThrow(() -> new IllegalStateException(OWNERSHIP_INFO_MAP_NULL_MESSAGE))
									.addEntryListener(new EntryRemovedListener<Pair<EntityId, ProjectLockCategory>, Void>() {

			@Override
			public void entryRemoved(final @Nullable EntryEvent<Pair<EntityId, ProjectLockCategory>, Void> event) {
				if (assertNotNull(event).getKey().equals(new Pair<>(projectId, lockCategory))) {
					tryLockAndRun(callbackCalled, listenerIdRef.get(), projectId, lockCategory, reasonPhrase, lockConsumer);
				}
			}
		}, false);
		listenerIdRef.set(listenerId);

		tryLockAndRun(callbackCalled, listenerId, projectId, lockCategory, reasonPhrase, lockConsumer);
	}

	private void tryLockAndRun(final AtomicBoolean callbackCalled, final UUID listenerId, final EntityId projectId, final ProjectLockCategory lockCategory,
			final String reasonPhrase, final Consumer<ProjectLock> lockConsumer) {

		final String userId = authenticationFacade.getUserId();
		final String userName = lookupUserName(userId);
		final ProjectLockOwnershipInfo ownershipInfo = new ProjectLockOwnershipInfo(userId, userName, reasonPhrase);
		final Optional<ProjectLock> projectLock = tryLockAndUpdateOwnership(projectId, lockCategory, ownershipInfo);
		if (projectLock.isPresent()) {
			if (callbackCalled.compareAndSet(false, true)) {
				/* about to call the callback -> map listener no longer required */
				Optional.ofNullable(ownershipInfoMap).orElseThrow(() -> new IllegalStateException(OWNERSHIP_INFO_MAP_NULL_MESSAGE))
						.removeEntryListener(listenerId);

				lockConsumer.accept(projectLock.get());
			} else {
				/* callback already called -> release lock and do nothing otherwise */
				projectLock.get().unlock();
			}
		}
	}

	@SuppressWarnings("java:S2222") /* the lock is returned to the caller, so it can't be unlocked here */
	private Optional<ProjectLock> tryLockAndUpdateOwnership(final EntityId projectId, final ProjectLockCategory lockCategory,
			final ProjectLockOwnershipInfo ownershipInfo) {
		final FencedLock lock = lockManager.getLock("ProjectLock-" + projectId + "-" + lockCategory);
		if (lock.tryLock(DEFAULT_LOCK_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
			Optional.ofNullable(ownershipInfoMap).orElseThrow(() -> new IllegalStateException(OWNERSHIP_INFO_MAP_NULL_MESSAGE))
					.put(new Pair<>(projectId, lockCategory), ownershipInfo);
			return Optional.of(new ProjectLock(projectId, lockCategory, lock));
		} else {
			return Optional.empty();
		}
	}

	private String lookupUserName(final String userId) {
		final UserNameService.LookupResult lookupResult = userNameService.lookupUserName(userId);
		if (lookupResult.isMissing()) {
			return "Unknown User";
		} else {
			return lookupResult.getUserName();
		}
	}
}
