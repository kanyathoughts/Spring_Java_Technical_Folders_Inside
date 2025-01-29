package strategy;

public class Demo {
    public static void main(String[] args) {
        // Initially we have set the pricing strategy as distance based but later we can
        // change the algorithm at runtime
        TripCalculator tripCalculator = new TripCalculator(new DistancePricing());
        double cost = tripCalculator.calculateTripCost(100);
        System.out.println("Distance pricing: " + cost);

        // Switching to duration pricing strategy
        tripCalculator.setPricingStrategy(new DurationPricing());
        cost = tripCalculator.calculateTripCost(100);
        System.out.println("Duration pricing: " + cost);

        // Switching to flat rate pricing strategy
        tripCalculator.setPricingStrategy(new FlatRatePricing());
        cost = tripCalculator.calculateTripCost(100);
        System.out.println("flat rate pricing: " + cost);
    }

}
