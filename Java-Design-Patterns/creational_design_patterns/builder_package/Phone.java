package builder_package;

public class Phone {

    private String brand;
    private String os;
    private int screensize;
    private int ram;
    private int storage;

    public Phone(String brand, String os, int screensize, int ram, int storage) {
        this.brand = brand;
        this.os = os;
        this.screensize = screensize;
        this.ram = ram;
        this.storage = storage;
    }

    @Override
    public String toString() {
        return "Phone [brand=" + brand + ", os=" + os + ", screensize=" + screensize + ", ram=" + ram + ", storage="
                + storage + "]";
    }

}
