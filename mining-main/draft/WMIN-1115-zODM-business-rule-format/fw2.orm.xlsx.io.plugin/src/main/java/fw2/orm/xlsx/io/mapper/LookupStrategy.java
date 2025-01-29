package fw2.orm.xlsx.io.mapper;

import java.util.List;

import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.Repository;
import fw2.orm.xlsx.io.WorkbookReader;

public interface LookupStrategy {

	Object lookupObject(String name, List<NameValuePair> lookupKeys);

	void init(Repository repository, WorkbookReader<?> workbookReader);

}
