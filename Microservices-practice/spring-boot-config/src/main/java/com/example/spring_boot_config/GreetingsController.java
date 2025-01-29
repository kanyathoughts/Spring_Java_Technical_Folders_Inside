package com.example.spring_boot_config;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RefreshScope
public class GreetingsController {
	
	
	@Value("${my.greeting: default value}")
	private String greetingMessage;
//	
//	@Value("Some static text")
//	private String staticText;
//	
//	@Value("${my.list.values}")
//	private List<String> listValues;
//	
//	// Here we have given #{} which denotes spring expression language (SpEL) and basically ${dbvalues} returns the String but
//	// if we mention inside the #{} then it will consider this as SpEL so it will return proper key,value pair values which is map values
//	@Value("#{${dbvalues}}")
//	private Map<String, String> dbValues;
//	
	@Autowired
	private DBSettings dbSettings;
//	
//	@Autowired
//	private Environment env;
	
	@GetMapping("/greeting")
	public String getGreeting() {
		return greetingMessage;
	}
	
//	@GetMapping("/greeting1")
//	public List<String> getGreeting1() {
//		return listValues;
//	}
//	
//	@GetMapping("/greeting2")
//	public Map<String, String> getGreeting2() {
//		return dbValues;
//	}
//	
	@GetMapping("/greeting3")
	public String getGreeting3() {
		return dbSettings.getConnection() + dbSettings.getHost();
	}
//	
//	@GetMapping("/envDetails")
//	public String getEnvDetails() {
//		return env.getProperty("my.greeting");
//	}

}
