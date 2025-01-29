package proxy;

public class EmployeeDAOImplProxy implements EmployeeDAO {

    EmployeeDAOImpl employeeDAOImpl;

    public EmployeeDAOImplProxy(EmployeeDAOImpl employeeDAOImpl) {
        this.employeeDAOImpl = employeeDAOImpl;
    }

    @Override
    public void createEmployee(String client, Employee employee) {
        if (client.equals("ADMIN")) {
            employeeDAOImpl.createEmployee(client, employee);
        } else {
            System.out.println("Access Denied");
        }
    }

    @Override
    public void deleteEmployee(String client, int id) {
        if (client.equals("ADMIN")) {
            employeeDAOImpl.deleteEmployee(client, id);
        } else {
            System.out.println("Access Denied");
        }
    }

    @Override
    public void updateEmployee(String client, Employee employee) {
        if (client.equals("ADMIN") || client.equals("USER")) {
            employeeDAOImpl.updateEmployee(client, employee);
        } else {
            System.out.println("Access Denied");
        }
    }

}
