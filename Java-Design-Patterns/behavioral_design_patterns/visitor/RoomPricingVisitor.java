package visitor;

public class RoomPricingVisitor implements RoomVisitor {

    @Override
    public void visit(SingleRoom singleRoom) {
        System.out.println("Single room pricing");
        singleRoom.roomPrice = 1000;
    }

    @Override
    public void visit(DoubleRoom doubleRoom) {
        System.out.println("Double room pricing");
        doubleRoom.roomPrice = 3000;
    }

    @Override
    public void visit(DeluxRoom deluxRoom) {
        System.out.println("Delux room pricing");
        deluxRoom.roomPrice = 5000;
    }

}
