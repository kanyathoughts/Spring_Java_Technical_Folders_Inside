package factory_package_another_example;

public class ClientTest {
    public static void main(String[] args) {
        // Factory method way is VehicleFactory will instantiate the classes and client just need to specify the vehicle name
        Vehicle vehicle = VehicleFactory.getVehicle("car");
        vehicle.drive();

        // Normal way is instantiating the classes 
        Vehicle vehicle2 = new Car();
        vehicle2.drive();
    }
}
