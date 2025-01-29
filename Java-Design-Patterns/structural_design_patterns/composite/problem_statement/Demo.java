package composite.problem_statement;

public class Demo {

    public static void main(String[] args) {
        Directory d1 = new Directory("Movie");
        File f1 = new File("Border");
        d1.add(f1);

        Directory d2 = new Directory("Comedy Movie");
        File f2 = new File("Hulchul");
        d2.add(f2);
        d1.add(d2);

        d1.ls();
    }

}
