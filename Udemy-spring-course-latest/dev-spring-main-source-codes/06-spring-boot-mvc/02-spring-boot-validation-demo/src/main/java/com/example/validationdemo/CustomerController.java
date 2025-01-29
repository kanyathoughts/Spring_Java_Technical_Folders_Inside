package com.example.validationdemo;

import org.springframework.beans.propertyeditors.StringTrimmerEditor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;

import jakarta.validation.Valid;

@Controller
public class CustomerController {
	
	//we are adding @InitBinder to trim input strings
	//	@InitBinder annotation is actually a pre processer
	//It will pre-process all web requests coming to controller

	@InitBinder
	public void initBinder(WebDataBinder dataBinder) {
		StringTrimmerEditor stringTrimmerEditor = new StringTrimmerEditor(true);
		dataBinder.registerCustomEditor(String.class, stringTrimmerEditor);
		
	}
	
	@GetMapping("/")
	public String showForm(Model model) {
		
		//model allows us to share information between controller and view pages
		
		//add Customer class object to model
		model.addAttribute("customer", new Customer());
		
		return "customer-form";
	}
	
	@PostMapping("/processForm")
	public String processForm(@Valid @ModelAttribute("customer") Customer theCustomer, BindingResult bindingResult) {
		
		//here it will print the binding result like it has any errors and if present what are the codes for them which we will use in messages.properties file to over ride the default error message with our custom message.
		
		//Binding Result: org.springframework.validation.BeanPropertyBindingResult: 1 errors
		//Field error in object 'customer' on field 'freePass': rejected value [kanua]; codes [typeMismatch.customer.freePass,typeMismatch.freePass,typeMismatch.java.lang.Integer,typeMismatch]; 
		//arguments [org.springframework.context.support.DefaultMessageSourceResolvable: codes [customer.freePass,freePass]; arguments []; default message [freePass]]; default message [Failed to convert property value of type 
		//'java.lang.String' to required type 'java.lang.Integer' for property 'freePass'; For input string: "kanua"]
		System.out.println("Binding Result: " + bindingResult.toString());
		
		System.out.println("\n\n\n\n");
		
		if (bindingResult.hasErrors()) {
			return "customer-form";
		} else {
			return "customer-confirmation";
		}
		
	}

}
