package decorator;

public class Client {
    public static void main(String[] args) {
        NormalCoffeeMachine normal = new NormalCoffeeMachine();
        CoffeeMachine enhanced = new NormalCoffeeMachineDecorator(normal);

        normal.makeSmallCoffee();
        normal.makeLargeCoffee();
        enhanced.makeSmallCoffee();
        enhanced.makeLargeCoffee();
    }
}
