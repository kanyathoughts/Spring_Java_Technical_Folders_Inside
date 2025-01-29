package observer;

public class Investor implements Observer {
    String name;

    public Investor(String name) {
        this.name = name;
    }

    @Override
    public void update(String stockName, double stockPrice) {
        System.out.println(name + " is updated: " + stockName + " with price: " + stockPrice);
    }

}
