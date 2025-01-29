package composite.solution_Using_Composite_Pattern;

import java.util.ArrayList;
import java.util.List;

public class Directory implements FileSystem {

    private String directoryName;
    List<FileSystem> fileSystemList;

    public Directory(String directoryName) {
        this.directoryName = directoryName;
        this.fileSystemList = new ArrayList<>();
    }

    // Adding file or directory
    // because both are implementing classes of FileSystem so both can be accepted
    // as we can add both files and directories inside a directory
    public void add(FileSystem fileSystemObject) {
        fileSystemList.add(fileSystemObject);
    }

    @Override
    public void ls() {
        System.out.println("Directory name: " + directoryName);
        for (FileSystem fs : fileSystemList) {
            fs.ls();
        }
    }

}
