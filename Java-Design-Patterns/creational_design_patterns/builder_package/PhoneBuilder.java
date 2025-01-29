package builder_package;

public class PhoneBuilder {
    private String brand;
    private String os;
    private int screensize;
    private int ram;
    private int storage;

    public PhoneBuilder setBrand(String brand) {
        this.brand = brand;
        return this;
    }

    public PhoneBuilder setOs(String os) {
        this.os = os;
        return this;
    }

    public PhoneBuilder setScreensize(int screensize) {
        this.screensize = screensize;
        return this;
    }

    public PhoneBuilder setRam(int ram) {
        this.ram = ram;
        return this;
    }

    public PhoneBuilder setStorage(int storage) {
        this.storage = storage;
        return this;
    }

    public Phone getPhone() {
        return new Phone(brand, os, screensize, ram, storage);
    }

}
