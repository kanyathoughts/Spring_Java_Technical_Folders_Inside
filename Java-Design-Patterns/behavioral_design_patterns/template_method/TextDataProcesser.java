package template_method;

public class TextDataProcesser extends DataProcesser {

    @Override
    public void loadData() {
        System.out.println("Loading text data");
    }

    @Override
    public void processData() {
        System.out.println("Processing text data");
    }

    @Override
    public void saveResult() {
        System.out.println("Saving final text data result");
    }

}
