package chainOfResponsibility;

public class DebugLogger extends Logger {

    public DebugLogger() {
        this.level = DEBUG;
    }

    @Override
    public void write(String message) {
        System.out.println("Debug: " + message);
    }

}
