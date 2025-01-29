package mediator;

public class Demo {
    public static void main(String[] args) {
        Mediator mediator = new AuctionMediator();
        Bidder b1 = new Bidder("Kanya", mediator);
        Bidder b2 = new Bidder("Akhil", mediator);
        Bidder b3 = new Bidder("Jashu", mediator);
        Bidder b4 = new Bidder("Tattu", mediator);

        b1.placeBid(100);

    }

}
