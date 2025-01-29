package proxy;

public class EmployeeDAOImpl implements EmployeeDAO {

    @Override
    public void createEmployee(String client, Employee employee) {
        System.out.println("New employee created in employee table");
    }

    @Override
    public void deleteEmployee(String client, int id) {
        System.out.println("Employee with id: " + id + " is deleted");
    }

    @Override
    public void updateEmployee(String client, Employee employee) {
        System.out.println("Employee updated");
    }

}
