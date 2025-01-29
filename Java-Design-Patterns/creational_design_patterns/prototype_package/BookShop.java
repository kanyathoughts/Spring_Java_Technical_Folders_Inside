package prototype_package;

import java.util.ArrayList;
import java.util.List;

public class BookShop implements Cloneable {

    private String bookShop;
    List<Book> books = new ArrayList<>();

    public String getBookShop() {
        return bookShop;
    }

    public void setBookShop(String bookShop) {
        this.bookShop = bookShop;
    }

    public List<Book> getBooks() {
        return books;
    }

    public void setBooks(List<Book> books) {
        this.books = books;
    }

    // Just assume we are loading data from database and which takes time
    // So when you create object, it will first load data from database
    public void loadData() {
        for (int i = 1; i <= 10; i++) {
            Book b = new Book();
            b.setBid(i);
            b.setBname("Book " + i);
            getBooks().add(b);
        }
    }

    // This is for shallow cloning
    // @Override
    // protected Object clone() throws CloneNotSupportedException {
    // return super.clone();
    // }

    @Override
    protected BookShop clone() throws CloneNotSupportedException {
        BookShop shop = new BookShop();

        for (Book b : this.getBooks()) {
            Book newBook = new Book();
            newBook.setBid(b.getBid());
            newBook.setBname(b.getBname());
            shop.getBooks().add(newBook);
        }

        return shop;
    }

    @Override
    public String toString() {
        return "BookShop [bookShop=" + bookShop + ", books=" + books + "]";
    }

}
