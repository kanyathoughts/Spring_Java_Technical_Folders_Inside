package observer;

import java.util.ArrayList;
import java.util.List;

public class Stock implements Subject {
    private String stockName;
    private double stockPrice;
    private List<Observer> observers;

    public Stock(String stockName) {
        this.stockName = stockName;
        observers = new ArrayList<>();
    }

    public void setStockPrice(double stockPrice) {
        this.stockPrice = stockPrice;
        notifyObservers();
    }

    @Override
    public void registerObserver(Observer observer) {
        observers.add(observer);
    }

    @Override
    public void unregisterObserver(Observer observer) {
        observers.remove(observer);
    }

    @Override
    public void notifyObservers() {
        for (Observer observer : observers) {
            observer.update(stockName, stockPrice);
        }
    }

}
