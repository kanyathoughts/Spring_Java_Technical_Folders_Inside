package iterator;

import java.util.ArrayList;
import java.util.List;

public class Demo {
    public static void main(String[] args) {
        List<Book> books = new ArrayList<>();
        books.add(new Book("Telugu", 100));
        books.add(new Book("Maths", 200));
        books.add(new Book("English", 300));

        Iterator iterator = new BookIterator(books);
        while (iterator.hasNext()) {
            Book book = (Book) iterator.next();
            System.out.println(book.bookName);
        }
    }
}
