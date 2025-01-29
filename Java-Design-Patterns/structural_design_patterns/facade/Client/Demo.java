package facade.Client;

import facade.Facade.OrderFacade;

public class Demo {

    public static void main(String[] args) {

        // Here Client is directly interacting with facade only and facade can take care
        // of the remaining process of calling methods inside other calsses
        OrderFacade of = new OrderFacade();
        of.createOrder();
    }

}
