package template_method;

abstract public class DataProcesser {

    public abstract void loadData();

    public abstract void processData();

    public abstract void saveResult();

    // Template method
    public final void processWholeData() {
        loadData();
        processData();
        saveResult();
    }

}
