package composite.solution_Using_Composite_Pattern;

public class File implements FileSystem {
    private String fileName;

    public File(String fileName) {
        this.fileName = fileName;
    }

    @Override
    public void ls() {
        System.out.println("File Name:  " + fileName);
    }

}
