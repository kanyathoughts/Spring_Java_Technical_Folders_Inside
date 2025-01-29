package factory_package_another_example;

public class VehicleFactory {
    public static Vehicle getVehicle(String vehicle) {
        if (vehicle.equals("car")) {
            return new Car();
        } else if (vehicle.equals("bike")) {
            return new Bike();
        } else {
            throw new IllegalArgumentException("unknown vehicle");
        }
    }
}
