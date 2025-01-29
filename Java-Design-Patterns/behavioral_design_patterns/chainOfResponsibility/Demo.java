package chainOfResponsibility;

public class Demo {

    public static void main(String[] args) {
        Logger infoLogger = new InfoLogger();
        Logger errorLogger = new ErrorLogger();
        Logger debuLogger = new DebugLogger();

        // setting up the chain of handlers.
        infoLogger.setNextLogger(errorLogger);
        errorLogger.setNextLogger(debuLogger);

        infoLogger.logMessage(Logger.INFO, "This is info");
        infoLogger.logMessage(Logger.ERROR, "This is error");
        errorLogger.logMessage(Logger.DEBUG, "This is debug");
    }

}
