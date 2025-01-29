package template_method;

public class Demo {

    public static void main(String[] args) {
        DataProcesser texDataProcesser = new TextDataProcesser();
        texDataProcesser.processWholeData();

        System.out.println("-------------------------------------------");
        DataProcesser csvDataProcesser = new CSVDataProcesser();
        csvDataProcesser.processWholeData();
    }

}
