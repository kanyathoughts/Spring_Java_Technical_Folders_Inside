package visitor;

public interface RoomVisitor {
    public void visit(SingleRoom singleRoom);

    public void visit(DoubleRoom doubleRoom);

    public void visit(DeluxRoom deluxRoom);
}
