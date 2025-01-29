package chainOfResponsibility;

abstract class Logger {
    public static int INFO = 1;
    public static int DEBUG = 2;
    public static int ERROR = 3;

    public int level;
    public Logger nextLogger;

    public void setNextLogger(Logger nextLogger) {
        this.nextLogger = nextLogger;
    }

    public void logMessage(int level, String message) {
        // If current object level is less than or equal to the passed level then
        // process the request.
        // else if next logger is available then pass the request to the next handler.
        // it will process the request
        if (this.level <= level) {
            write(message);
        }

        if (nextLogger != null) {
            nextLogger.logMessage(level, message);
        }
    }

    // Since this is abstract class we have to mention abstarct keyword
    public abstract void write(String message);

}
