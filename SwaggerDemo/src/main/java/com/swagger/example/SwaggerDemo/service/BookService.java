package com.swagger.example.SwaggerDemo.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.swagger.example.SwaggerDemo.dao.BookRepository;
import com.swagger.example.SwaggerDemo.model.Book;

@Service
public class BookService {

	@Autowired
	private BookRepository bookRepository;

	public String saveBook(Book book) {
		bookRepository.save(book);
		return "book saved with id: " + book.getBookId();
	}

	public Book getBook(int bookId) {
		return bookRepository.findById(bookId).orElse(null);
	}

	public List<Book> removeBook(int bookId) {
		bookRepository.deleteById(bookId);
		return bookRepository.findAll();
	}

}
