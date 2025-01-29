package com.swagger.example.SwaggerDemo.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.swagger.example.SwaggerDemo.model.Book;
import com.swagger.example.SwaggerDemo.service.BookService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@RestController
@RequestMapping("/book")
@Api(value = "Book service", description = "This is book service")
public class BookController {
	
	@Autowired
	private BookService bookService;
	
	@PostMapping("/save")
	@ApiOperation("Store book api")
	public String saveBook(@RequestBody Book book) {
		return bookService.saveBook(book);
	}
	
	@GetMapping("/searchBook/{bookId}")
	public Book getBook(@PathVariable int bookId) {
		return bookService.getBook(bookId);
	}
	
	@DeleteMapping("/deleteBook/{bookId}")
	public List<Book> removeBook(@PathVariable int bookId) {
		return bookService.removeBook(bookId);
	}

}
