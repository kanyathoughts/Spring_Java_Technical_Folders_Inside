/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Scanner;
import java.util.UUID;

import org.springframework.stereotype.Component;

import com.hazelcast.core.HazelcastInstance;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.config.properties.ClusterProperties.NodeNameStrategy;
import software.amazon.awssdk.regions.internal.util.EC2MetadataUtils;

/**
 * This factory can be used to create the cluster node name depending on the {@link NodeNameFactory}.
 */
@Component
public class NodeNameFactory {

	/**
	 * Creates the cluster node name depending on the provided {@code strategy}.
	 * 
	 * @param strategy the {@link NodeNameStrategy}
	 * @param hz the {@link HazelcastInstance} required when {@link NodeNameStrategy#INTERNAL_NODE_ID} is being used
	 * @return the cluster node name
	 */
	public String createNodeName(final NodeNameStrategy strategy, @Nullable final HazelcastInstance hz) {
		final String name;
		switch (strategy) {
			case RANDOM_UUID:
				name = UUID.randomUUID().toString();
				break;
			case INTERNAL_NODE_ID:
				name = Assert.assertNotNull(hz, "A hazelcast instance must be provided for the strategy " + strategy.name())
					.getCluster().getLocalMember().getUuid().toString();
				break;
			case EC2_INSTANCE_ID:
				name = EC2MetadataUtils.getInstanceId();
				break;
			case IP:
				/* Resolves the primary outbound IP address, in case there are multiple network interfaces.
				 * 8.8.8.8 must not be reachable for this to work. */
				try (final DatagramSocket socket = new DatagramSocket()) {
					socket.connect(InetAddress.getByName("8.8.8.8"), 10002);
					name = socket.getLocalAddress().getHostAddress();
				} catch (final SocketException | UnknownHostException e) {
					throw new IllegalStateException("Unable to resolve primary outbound IP address.", e);
				}
				break;
			case HOSTNAME:
				try (final Scanner s = new Scanner(Runtime.getRuntime().exec("hostname").getInputStream())) {
					name = s.hasNext() ? s.next() : "<unknown-hostname>";
				} catch (final IOException e) {
					throw new IllegalStateException("Unable to resolve hostname.", e);
				}
				break;
			default:
				throw new IllegalArgumentException("Unsupported node name strategy: " + strategy);
		}
		
		return name;
	}
}
