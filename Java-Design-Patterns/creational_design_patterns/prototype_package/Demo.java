package prototype_package;

public class Demo {

    public static void main(String[] args) throws CloneNotSupportedException {
        BookShop bs = new BookShop();
        bs.setBookShop("Akhil_Bookshop");
        bs.loadData();
        System.out.println("bs: " + bs.toString());

        System.out.println(
                "========================================================================================================");
        // if you do like this again you are fetching same data from database
        // so you can copy the values from bs object so we can do cloning here.
        // BookShop bs2 = new BookShop();
        // bs2.setBookShop("Kanya_Bookshop");
        // bs2.loadData();
        // System.out.println(bs2.toString());

        // clone method is from Object but it is protected and we can't use it outside
        // package.
        // So to make use of clone method, we (BookShop) have to implement cloneable
        // marker interface
        // and override the clone method and here you just did nothing just by simply
        // calling Object class clone method.
        // Here we are doing shollow cloning means if you change anything inside the bs
        // object that will be reflected in bs2 object

        BookShop bs2 = bs.clone();
        bs2.setBookShop("Kanya_Bookshop");
        System.out.println("bs2: " + bs2.toString());
        bs.getBooks().get(2).setBname("This is 3rd book");
        System.out.println(
                "========================================================================================================");
        System.out.println("bs after updating book name: " + bs.toString());

        System.out.println(
                "========================================================================================================");
        System.out.println("bs2 after updating book name in bs: " + bs2.toString());

    }
}
