package proxy;

public class Demo {
    public static void main(String[] args) {
        EmployeeDAO employeeDAO = new EmployeeDAOImplProxy(new EmployeeDAOImpl());
        employeeDAO.createEmployee("USER", new Employee(1, "Kanya`", "female"));
        employeeDAO.createEmployee("ADMIN", new Employee(1, "Kanya`", "female"));
    }
}
