package iterator;

import java.util.List;

public class BookIterator implements Iterator {

    List<Book> bookList;
    int index = 0;

    public BookIterator(List<Book> bookList) {
        this.bookList = bookList;
    }

    @Override
    public boolean hasNext() {
        // If index is less than books size then it means we have next element
        return (index < bookList.size());
    }

    @Override
    public Object next() {
        if (this.hasNext()) {
            // This will actually get the element at index value and increment the index
            // value
            return bookList.get(index++);
        }
        return null;
    }

}
