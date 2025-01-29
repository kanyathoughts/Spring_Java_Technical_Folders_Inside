package builder_package;

public class PhoneBuilder_Test {
    public static void main(String[] args) {
        // Normal way to initialize all the values
        Phone p1 = new Phone("Apple", "ios", 6, 6, 128);
        System.out.println(p1.toString());

        // Normal way to initialize only brand and ram
        Phone p2 = new Phone("Apple", null, 0, 6, 0);
        System.out.println(p2.toString());

        // builder pattern way to initialize brand and ram and at the end we are getting
        // phone object.
        Phone p3 = new PhoneBuilder().setBrand("Samsung").setRam(8).getPhone();
        System.out.println(p3.toString());

    }

}
