package com.example.unitTesting.UnitTesting.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.example.unitTesting.UnitTesting.model.Item;

@Repository
public interface ItemRepository extends JpaRepository<Item, Integer>{

}
