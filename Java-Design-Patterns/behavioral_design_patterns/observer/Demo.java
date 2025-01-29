package observer;

public class Demo {
    public static void main(String[] args) {
        Stock appleStock = new Stock("Apple");

        Observer observer1 = new Investor("Kanya");
        Observer observer2 = new Investor("Akhil");

        // register observers
        appleStock.registerObserver(observer1);
        appleStock.registerObserver(observer2);

        // updating/setting stock price
        appleStock.setStockPrice(200.00); // Both observer1 and observer2 will be notified
        appleStock.setStockPrice(196.74);

        // un registering observer 2
        appleStock.unregisterObserver(observer2);

        appleStock.setStockPrice(155.87); // Only observer2 will be notified

    }
}
