package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

public interface InstanceFactory {

	<T> T createInstance(Class<T> type, List<NameValuePair> row) throws Exception;

	<T> List<T> createInstances(Class<T> type, List<ArrayList<NameValuePair>> rows) throws Exception;



}