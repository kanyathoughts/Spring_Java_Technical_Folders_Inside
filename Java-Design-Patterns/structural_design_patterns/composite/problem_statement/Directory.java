package composite.problem_statement;

import java.util.ArrayList;
import java.util.List;

public class Directory {

    private String directoryName;
    List<Object> objectsList;

    public Directory(String directoryName) {
        this.directoryName = directoryName;
        this.objectsList = new ArrayList<>();
    }

    public void add(Object obj) {
        objectsList.add(obj);
    }

    public void ls() {
        System.out.println("Dircetory name: " + directoryName);
        for (Object object : objectsList) {
            if (object instanceof File) {
                ((File) object).ls();
            } else if (object instanceof Directory) {
                ((Directory) object).ls();
            }
        }
    }

}
