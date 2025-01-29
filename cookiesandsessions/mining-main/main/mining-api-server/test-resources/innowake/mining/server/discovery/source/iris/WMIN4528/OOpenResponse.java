package WMIN4528;

import WMIN4528.ORawPair;

public class OOpenResponse implements OBinaryResponse {

  @Override
  public void write(OChannelDataOutput channel, int protocolVersion, ORecordSerializer serializer)
      throws IOException {
    channel.writeInt(sessionId);
    if (protocolVersion > OChannelBinaryProtocol.PROTOCOL_VERSION_26)
      channel.writeBytes(sessionToken);

    OMessageHelper.writeClustersArray(
        channel, new ORawPair<>(clusterNames, clusterIds), protocolVersion);
    channel.writeBytes(distributedConfiguration);
    channel.writeString(serverVersion);
  }

  @Override
  public void read(OChannelDataInput network, OStorageRemoteSession session) throws IOException {
    sessionId = network.readInt();
    sessionToken = network.readBytes();
    final ORawPair<String[], int[]> clusters = OMessageHelper.readClustersArray(network);
    distributedConfiguration = network.readBytes();
    serverVersion = network.readString();
  }

}
