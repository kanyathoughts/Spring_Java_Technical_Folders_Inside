package strategy;

public class FlatRatePricing implements PricingStrategy {

    @Override
    public double calculatePrice(int distance) {
        return 1000; // fixed price
    }

}
