/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.customer;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.repository.ProductRepository;

/**
 * Test case for  {@link ProductRepository}.
 */
public class ProductIntegrationTests extends AbstractIntegrationTests {
	
	@Autowired
	private ProductRepository productRepository;
	
	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Product");
	}
	
	/**
	 * Test case for byte array.
	 *
	 * @throws IOException exception during file read or write if there is any error.
	 */
	@Test
	public void testByteArrayData() throws IOException {
		final Product product1 = new Product("App Mod", Long.valueOf(1));
		final Path DIRECTORY_PATH = Paths.get("src", "test", "resources", "images");
		final File file = new File(DIRECTORY_PATH + File.separator +  "AppMod-logo.PNG");
		final byte[] imageArray = FileUtils.readFileToByteArray(file);
		product1.setProductImage(imageArray);
		
		final Product product = productRepository.save(product1);
		assertTrue(new ReflectionEquals(product1, "rid", "productId").matches(product));
		assertEqual(imageArray, product.getProductImage());
		
		final List<Product> fetchedProducts =  (List<Product>) productRepository.findAll();
		final Product fetchedProduct = fetchedProducts.get(0);
		assertEqual(imageArray, fetchedProducts.get(0).getProductImage());
		final File fetchedFile = new File(DIRECTORY_PATH + File.separator +  "AppMod-logo-fetched.PNG");
		FileUtils.writeByteArrayToFile(fetchedFile, fetchedProducts.get(0).getProductImage());
		
		final byte[] emtpy = new byte[0];
		fetchedProduct.setProductImage(emtpy);
		final Product emptyImage = productRepository.save(fetchedProduct);
		assertTrue(assertNotNull(emptyImage.getProductImage()).length == 0);
		final File emptyFile = new File(DIRECTORY_PATH + File.separator +  "AppMod-logo-empty.PNG");
		FileUtils.writeByteArrayToFile(emptyFile, emptyImage.getProductImage());
	}

}
