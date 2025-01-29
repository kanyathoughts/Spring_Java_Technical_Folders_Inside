package visitor;

public interface RoomElement {
    public void accept(RoomVisitor roomVisitor);
}
