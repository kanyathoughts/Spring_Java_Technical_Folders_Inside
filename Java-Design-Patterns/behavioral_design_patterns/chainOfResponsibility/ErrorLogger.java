package chainOfResponsibility;

public class ErrorLogger extends Logger {

    public ErrorLogger() {
        this.level = ERROR;
    }

    @Override
    public void write(String message) {
        System.out.println("Error: " + message);
    }

}
