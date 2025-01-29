package decorator;

public class NormalCoffeeMachine implements CoffeeMachine {

    @Override
    public void makeSmallCoffee() {
        System.out.println("Normal coffee machine: making small coffee");
    }

    @Override
    public void makeLargeCoffee() {
        System.out.println("Normal coffee machine: making large coffee");
    }

}
