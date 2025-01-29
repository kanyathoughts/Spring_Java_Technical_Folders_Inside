package com.swagger.example.SwaggerDemo.dao;

import org.springframework.data.jpa.repository.JpaRepository;

import com.swagger.example.SwaggerDemo.model.Book;

public interface BookRepository extends JpaRepository<Book, Integer>{

	void deleteById(int bookId);

}
