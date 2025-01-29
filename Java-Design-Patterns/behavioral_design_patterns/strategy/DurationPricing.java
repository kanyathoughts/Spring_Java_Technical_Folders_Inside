package strategy;

public class DurationPricing implements PricingStrategy {

    @Override
    public double calculatePrice(int distance) {
        return distance * 100; // price per one hour
    }

}
