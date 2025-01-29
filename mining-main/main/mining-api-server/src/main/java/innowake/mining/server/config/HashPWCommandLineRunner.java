/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import java.nio.charset.Charset;
import java.util.Scanner;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.security.crypto.bcrypt.BCrypt;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;

/**
 * Used to generate the user password hash value from the command line.
 * Execute this handler by providing {@value #COMMAND} as single argument.
 */
@Component
public class HashPWCommandLineRunner implements CommandLineRunner {

	/**
	 * The command line argument required to execute this handler.
	 */
	public static final String COMMAND = "hashpw";
	
    @Autowired
    private ConfigurableApplicationContext context;
	
	@Override
	public void run(@Nullable final String... args) throws Exception {
		if (args != null && args.length == 1 && COMMAND.equals(args[0])) {
			System.out.println("Enter password to hash:");
			try (Scanner scanner = new Scanner(System.in, Charset.defaultCharset().name())) {
				System.out.println(BCrypt.hashpw(scanner.nextLine(), BCrypt.gensalt()));
				context.close();
			}
		}
	}

}
