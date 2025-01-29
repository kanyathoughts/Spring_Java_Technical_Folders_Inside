/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.config;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Import;
import org.springframework.data.repository.core.NamedQueries;

import innowake.spring.data.orientdb.repository.support.OrientRepositoryFactoryBean;

/**
 * Annotation to enable Orient repositories. Will scan the package of the annotated configuration class for Spring Data
 * repositories by default.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Import({OrientRepositoryRegistrar.class, OrientSpringConfiguration.class})
public @interface EnableOrientRepositories {

	/**
	 * Base packages to scan for annotated components. {@link #value()} is an alias for (and mutually exclusive with) this
	 * attribute. Use {@link #basePackageClasses()} for a type-safe alternative to String-based package names.
	 *
	 * @return packages that needs to be scanned for repository
	 */
	String[] basePackages() default {};

	/**
	 * Specifies which types are eligible for component scanning. Further narrows the set of candidate components from
	 * everything in {@link #basePackages()} to everything in the base packages that matches the given filter or filters.
	 *
	 * @return collection of {@link Filter} to be included during component scanning
	 */
	Filter[] includeFilters() default {};

	/**
	 * Specifies which types are not eligible for component scanning.
	 * 
	 * @return collection of {@link Filter} to be excluded during component scanning
	 */
	Filter[] excludeFilters() default {};

	/**
	 * Alias for the {@link #basePackages()} attribute. Allows for more concise annotation declarations e.g.:
	 * {@code @EnableOrientObjectRepositories("org.my.pkg")} instead of {@code @EnableOrientObjectRepositories(basePackages="org.my.pkg")}.
	 * 
	 * @return packages that need to scanned for repository
	 */
	String[] value() default {};

	/**
	 * Type-safe alternative to {@link #basePackages()} for specifying the packages to scan for annotated components. The
	 * package of each class specified will be scanned. Consider creating a special no-op marker class or interface in
	 * each package that serves no purpose other than being referenced by this attribute.
	 * 
	 * @return classes whose packages needs to be scanned
	 */
	Class<?>[] basePackageClasses() default {};

	/**
	 * A factory bean class for the application.
	 * 
	 * @return the {@link FactoryBean} class to be used for each repository instance. Defaults to
	 * {@link OrientRepositoryFactoryBean}.
	 */
	Class<?> repositoryFactoryBeanClass() default OrientRepositoryFactoryBean.class;

	/**
	 * {@link NamedQueries} location path.
	 * 
	 * @return path for the location of namedQueries file
	 */
	String namedQueriesLocation() default "";

	/**
	 * Custom repository implementation postfix.
	 * 
	 * @return the postfix to be used when looking up custom repository implementations. Defaults to {@literal Impl}. So
	 * for a repository named {@code PersonRepository} the corresponding implementation class will be looked up scanning
	 * for {@code PersonRepositoryImpl}.
	 */
	String repositoryImplementationPostfix() default "Impl";
	
	/**
	 * Configures whether nested repository-interfaces (e.g. defined as inner classes) should be discovered by the
	 * repositories infrastructure.
	 */
	boolean considerNestedRepositories() default false;
}
