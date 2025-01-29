package decorator;

public class NormalCoffeeMachineDecorator implements CoffeeMachine {

    NormalCoffeeMachine normalCoffeeMachine;

    public NormalCoffeeMachineDecorator(NormalCoffeeMachine normalCoffeeMachine) {
        this.normalCoffeeMachine = normalCoffeeMachine;
    }

    // with milk
    @Override
    public void makeSmallCoffee() {
        System.out.println("Making small coffee with milk");
        normalCoffeeMachine.makeSmallCoffee();
        System.out.println("Adding milk");
    }

    // with milk and suger
    public void makeLargeCoffeeWithMilkAndSuger() {
        System.out.println("Making large coffee with milk and sugar");
        normalCoffeeMachine.makeLargeCoffee();
        System.out.println("Adding milk and sugar");
    }

    // un altered behavior
    @Override
    public void makeLargeCoffee() {
        normalCoffeeMachine.makeLargeCoffee();
    }

}
