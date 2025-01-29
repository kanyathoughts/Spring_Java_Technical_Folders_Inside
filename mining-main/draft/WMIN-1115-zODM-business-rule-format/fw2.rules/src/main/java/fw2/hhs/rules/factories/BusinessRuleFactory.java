package fw2.hhs.rules.factories;
//
//import java.util.ArrayList;
//import java.util.List;
//
//import org.apache.commons.lang3.ClassUtils;
//
//import fw2.hhs.rules.BusinessRule;
//import fw2.orm.xlsx.ClassMap;
//import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.io.Factory;
import fw2.orm.xlsx.io.mapper.Mapper;
//import fw2.orm.xlsx.io.NameValuePair;
//import fw2.repository.FW2Repository;
//import fw2.repository.Repository;
//
public class BusinessRuleFactory extends Factory{

	public BusinessRuleFactory(Mapper mapper) {
		super(mapper);
		// TODO Auto-generated constructor stub
	}
//	public BusinessRuleFactory(Repository repository) {
//		super(repository);
//		// TODO Auto-generated constructor stub
//	}
//	@Override
//    public <T> T createInstance(Class<T> type, List<NameValuePair> row) throws Exception {
//        T result = (T) type.newInstance();
//        ClassMap map = FW2Repository.getInstance().getClassMap(ClassUtils.getShortClassName(type));
//        for (ColumnMap columnMap : map.getColumnMaps()) {
//            columnMap.populate(result, row, this.getRepository());
//        }
//        return result;
//    }
//	@Override
//    public <T> List<T> createInstances(Class<T> type, List<ArrayList<NameValuePair>> rows) throws Exception {
//        List<T> result = new ArrayList<T>();
//        ClassMap map = FW2Repository.getInstance().getClassMap(ClassUtils.getShortClassName(type));
//        for (List<NameValuePair> row : rows) {
//            T obj = (T) type.newInstance();
//           List<ColumnMap> columnMaps =  map.getColumnMaps();
//           for (ColumnMap columnMap : columnMaps) {
//               columnMap.populate(obj, row, this.getRepository());
//           }
//            
//          
//            result.add(obj);
//        }
//        return result;
//    }
//
}
