package adapter_package.adapter;

import adapter_package.adaptee.WeightInPounds;

public class WeightInKgsImple implements WeightInKgs {

    WeightInPounds weightInPounds;

    // Don't get confused, we can pass any kind of implementation in the client it
    // self
    public WeightInKgsImple(WeightInPounds weightInPounds) {
        this.weightInPounds = weightInPounds;
    }

    @Override
    public double getWeightInKgs() {
        return (weightInPounds.getWeightInPounds() * (0.45));
    }

}
