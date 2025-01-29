package abstarct_factory_package;

public class ClientTest {
    public static void main(String[] args) {
        // Vehicle factory is the top level factory and HundayiFactory and ToyotoFactory are implementing this Vehicle Factory
        // Once we get the car and bike holdings then we can use the methods of that
        VehicleFactory vehicle = new HundayiFactory();
        Car car = vehicle.createCar();
        Bike bike = vehicle.createBike();
        car.driving();
        bike.driving();

        // This is the normal way of doing without any abstraction
        // if we have more implementations of car then it will be very tough to the client to instantiate all of them if he needs all
        // So we need abstract factory method design pattern where Hundayi factory is created to handle all the hundayi related calsses.
        // And Toyoto factory created to handle all toyoto related classes.
        Car c1 = new HundayiCar();
        Bike b1 = new HundayiBike();
        Car c2 = new ToyotoCar();
        Bike b2 = new ToyotoBike();
        c1.driving();
        b1.driving();
        c2.driving();
        b2.driving();
    }
    
}
