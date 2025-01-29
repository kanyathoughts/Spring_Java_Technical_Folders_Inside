package abstarct_factory_package;

public class ToyotoFactory implements VehicleFactory{

    @Override
    public Car createCar() {
        return new ToyotoCar();
    }

    @Override
    public Bike createBike() {
       return new ToyotoBike();
    }
    
}
