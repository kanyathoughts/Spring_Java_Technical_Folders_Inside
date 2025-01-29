package prototype_package_another_example;

public class Teacher implements Prototype_Example {

    private String name;
    private String subject;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    @Override
    public String toString() {
        return "Teacher [name=" + name + ", subject=" + subject + "]";
    }

    public Teacher(String name, String subject) {
        this.name = name;
        this.subject = subject;
    }

    @Override
    public Prototype_Example clone_copy() {
        return new Teacher(name, subject);
    }

}
