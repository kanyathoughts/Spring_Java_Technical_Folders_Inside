package template_method;

public class CSVDataProcesser extends DataProcesser {

    @Override
    public void loadData() {
        System.out.println("Loading csv data");
    }

    @Override
    public void processData() {
        System.out.println("Processing csv data");
    }

    @Override
    public void saveResult() {
        System.out.println("Saving final csv data result");
    }

}
