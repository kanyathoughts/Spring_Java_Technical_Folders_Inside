package adapter_package.client;

import adapter_package.adaptee.WeightInPoundsForBabies;
import adapter_package.adapter.WeightInKgs;
import adapter_package.adapter.WeightInKgsImple;

public class Demo {

    public static void main(String[] args) {
        // we have taken adapter and which has WeightInPoundsForBabies implementation
        WeightInKgs weightInKgs = new WeightInKgsImple(new WeightInPoundsForBabies());
        double result = weightInKgs.getWeightInKgs();
        System.out.println(result);
    }

}
