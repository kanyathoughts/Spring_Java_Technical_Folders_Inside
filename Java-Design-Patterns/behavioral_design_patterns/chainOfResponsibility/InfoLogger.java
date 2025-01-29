package chainOfResponsibility;

public class InfoLogger extends Logger {

    public InfoLogger() {
        this.level = INFO;
    }

    @Override
    public void write(String message) {
        System.out.println("Info: " + message);
    }

}
