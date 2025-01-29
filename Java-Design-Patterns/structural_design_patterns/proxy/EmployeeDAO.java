package proxy;

public interface EmployeeDAO {

    public void createEmployee(String client, Employee employee);

    public void deleteEmployee(String client, int id);

    public void updateEmployee(String client, Employee employee);

}
