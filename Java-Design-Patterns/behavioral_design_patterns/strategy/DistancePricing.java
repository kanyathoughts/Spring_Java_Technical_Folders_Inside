package strategy;

public class DistancePricing implements PricingStrategy {

    @Override
    public double calculatePrice(int distance) {
        return distance * 5.0; // price per kilo meter
    }

}
