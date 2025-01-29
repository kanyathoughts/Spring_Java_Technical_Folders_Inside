package visitor;

public class Demo {
    public static void main(String[] args) {
        RoomElement singleRoomElement = new SingleRoom();
        RoomElement doubleRoomElement = new DoubleRoom();
        RoomElement deluxRoomElement = new DeluxRoom();

        RoomVisitor pricingRoomVisitor = new RoomPricingVisitor();
        singleRoomElement.accept(pricingRoomVisitor);

        System.out.println(((SingleRoom) singleRoomElement).roomPrice);
    }
}
