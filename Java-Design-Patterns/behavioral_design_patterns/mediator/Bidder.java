package mediator;

public class Bidder implements Colleague {

    String name;
    Mediator mediator;

    public Bidder(String name, Mediator mediator) {
        this.name = name;
        this.mediator = mediator;
        // Whenever we create bidder we need to add to the mediator colleagues list
        // to keep track of all the bidders
        mediator.addBidder(this);
    }

    @Override
    public void placeBid(int amount) {
        mediator.placeBid(this, amount);
    }

    @Override
    public void receiveNotification(int bidAmount) {
        System.out.println(
                "Bidder " + name + " receives notification from mediator that one bidder bid the ammount "
                        + bidAmount);
    }
}
