package mediator;

public interface Mediator {
    // We have to keep track of all the bidders so we need to add bidders
    public void addBidder(Colleague colleague);

    public void placeBid(Colleague colleague, int amount);

}
