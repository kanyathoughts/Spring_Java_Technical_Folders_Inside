package innowake.spring.data.orientdb.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.orientechnologies.orient.core.id.ORID;

import innowake.spring.data.orientdb.api.query.clause.OrientClause;

/**
 * Orient specific extension of {@link org.springframework.data.repository.PagingAndSortingRepository}.
 * @param <T> generic type to handle entity
 */
@NoRepositoryBean
public interface OrientRepository<T> extends PagingAndSortingRepository<T, String> {
	
	/**
	 * Returns whether an entity with the given id exists.
	 *
	 * @param id record id saved in database
	 * @return true if an entity with the given id exists, false otherwise.
	 */
	boolean existsById(final ORID id);
	
	/**
	 * Deletes the element with the RID @param id present in the database.
	 * 
	 * @param id record id
	 */
	void deleteById(final ORID id);
	
	/**
	 * Retrieves an entity by its id.
	 *
	 * @param id record id saved in database
	 * @return the entity with the given id or Optional#empty() if none found.
	 */
	Optional<T> findById(final ORID id);
	
	/**
	 * Returns the data retrieved by querying with the criteria as {@link OrientClause}.
	 *
	 * @param clause criteria to build query
	 * @param pageable defines page size and element size
	 * @param queryArguments arguments for the criteria clause
	 * @return the data retrieved by querying with the criteria mentioned as clause
	 */
	Page<T> findAll(final OrientClause clause, final Pageable pageable, final Object... queryArguments);
}
