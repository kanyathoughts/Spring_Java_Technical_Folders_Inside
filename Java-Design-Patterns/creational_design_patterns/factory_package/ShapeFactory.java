package factory_package;

public class ShapeFactory {
    public static Shape getFood(String shape) {
        if (shape == null)
            return null;
        switch (shape) {
            case "Round": return new Pizza();
            case "Cylinder": return new Burrito();
            default: throw new IllegalArgumentException("unknown shape");
        }
    }
    
}
