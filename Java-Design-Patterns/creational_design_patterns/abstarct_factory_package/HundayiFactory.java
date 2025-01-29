package abstarct_factory_package;

public class HundayiFactory implements VehicleFactory{

    @Override
    public Car createCar() {
       return new HundayiCar();
    }

    @Override
    public Bike createBike() {
       return new HundayiBike();
    }
    
}
