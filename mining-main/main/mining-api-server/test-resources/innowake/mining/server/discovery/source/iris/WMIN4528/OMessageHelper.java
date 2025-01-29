package WMIN4528;

import WMIN4528.ORawPair;

public class OMessageHelper {

  public static ORawPair<String[], int[]> readClustersArray(final OChannelDataInput network)
      throws IOException {
    final int tot = network.readShort();
    final String[] clusterNames = new String[tot];
    final int[] clusterIds = new int[tot];

    for (int i = 0; i < tot; ++i) {
      String clusterName = network.readString().toLowerCase(Locale.ENGLISH);
      final int clusterId = network.readShort();
      clusterNames[i] = clusterName;
      clusterIds[i] = clusterId;
    }

    return new ORawPair<>(clusterNames, clusterIds);
  }

  public static void writeClustersArray(
      OChannelDataOutput channel, ORawPair<String[], int[]> clusters, int protocolVersion)
      throws IOException {
    final String[] clusterNames = clusters.first;
    final int[] clusterIds = clusters.second;

    channel.writeShort((short) clusterNames.length);

    for (int i = 0; i < clusterNames.length; i++) {
      channel.writeString(clusterNames[i]);
      channel.writeShort((short) clusterIds[i]);
    }
  }
}
