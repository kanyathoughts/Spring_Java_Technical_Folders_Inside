package mediator;

public interface Colleague {
    // Every bidder can place the bid with amount
    public void placeBid(int amount);

    // He should receive notification from mediator when anyone places the bid
    public void receiveNotification(int bidAmount);
}
