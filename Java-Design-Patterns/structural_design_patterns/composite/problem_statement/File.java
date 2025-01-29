package composite.problem_statement;

public class File {

    private String fileName;

    public File(String fileName) {
        this.fileName = fileName;
    }

    public void ls() {
        System.out.println("File name: " + fileName);
    }

}
