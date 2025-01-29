package mediator;

import java.util.ArrayList;
import java.util.List;

public class AuctionMediator implements Mediator {
    List<Colleague> colleagues = new ArrayList<>();

    @Override
    public void addBidder(Colleague colleague) {
        colleagues.add(colleague);
    }

    @Override
    public void placeBid(Colleague bidder, int amount) {
        for (Colleague colleague : colleagues) {
            // mediator should send notification to all bidders except the bidder who placed
            // the bid
            if (colleague != bidder) {
                // when bid happens all other bidders should know about it.
                colleague.receiveNotification(amount);
            }
        }
    }

}
