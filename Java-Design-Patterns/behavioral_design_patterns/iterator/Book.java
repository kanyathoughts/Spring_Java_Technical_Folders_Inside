package iterator;

public class Book {
    String bookName;
    int bookPrice;

    public Book(String bookName, int bookPrice) {
        this.bookName = bookName;
        this.bookPrice = bookPrice;
    }

    public String getBookName() {
        return bookName;
    }

    public int getBookPrice() {
        return bookPrice;
    }

    @Override
    public String toString() {
        return "Book [bookName=" + bookName + ", bookPrice=" + bookPrice + "]";
    }

}
