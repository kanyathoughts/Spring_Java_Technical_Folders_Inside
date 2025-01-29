package strategy;

public class TripCalculator {
    PricingStrategy pricingStrategy;

    public void setPricingStrategy(PricingStrategy pricingStrategy) {
        this.pricingStrategy = pricingStrategy;
    }

    public TripCalculator(PricingStrategy pricingStrategy) {
        this.pricingStrategy = pricingStrategy;
    }

    public double calculateTripCost(int distance) {
        return pricingStrategy.calculatePrice(distance);
    }

}
