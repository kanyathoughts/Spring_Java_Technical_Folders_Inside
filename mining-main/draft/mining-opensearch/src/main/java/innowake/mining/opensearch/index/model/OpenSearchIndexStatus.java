package innowake.mining.opensearch.index.model;

public class OpenSearchIndexStatus {
    private String health;
    private String status;
    private String index;
    private String uuid;

    public String getHealth() {
        return health;
    }

    public void setHealth(final String health) {
        this.health = health;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(final String status) {
        this.status = status;
    }

    public String getIndex() {
        return index;
    }

    public void setIndex(final String index) {
        this.index = index;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(final String uuid) {
        this.uuid = uuid;
    }
}
