
package WMIN4528;

import WMIN4528.ORawPair;

public class OReloadResponse implements OBinaryResponse {

  @Override
  public void read(OChannelDataInput network, OStorageRemoteSession session) throws IOException {
    final ORawPair<String[], int[]> clusters = OMessageHelper.readClustersArray(network);
    clusterNames = clusters.first;
    clusterIds = clusters.second;
  }

  public void write(OChannelDataOutput channel, int protocolVersion, ORecordSerializer serializer)
      throws IOException {
    OMessageHelper.writeClustersArray(
        channel, new ORawPair<>(clusterNames, clusterIds), protocolVersion);
  }
}
