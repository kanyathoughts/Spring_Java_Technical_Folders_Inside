package fw2.orm.xlsx.io.mapper;

import java.util.List;

import fw2.orm.xlsx.io.NameValuePair;

import fw2.orm.xlsx.io.Repository;
import fw2.orm.xlsx.io.WorkbookReader;

public class RepositoryLookupStrategy implements LookupStrategy {

	private Repository repository;
	
	@Override
	public Object lookupObject(String name, List<NameValuePair> lookupKeys) {
		
		return repository.lookupObject(name,lookupKeys);
	}

	@Override
	public void init(Repository repository, WorkbookReader<?> workbookReader) {
		this.repository=repository;
		
	}

}
