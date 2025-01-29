package prototype_package_another_example;

public class Principal implements Cloneable {

    private String name;
    private String colleageName;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getColleageName() {
        return colleageName;
    }

    public void setColleageName(String colleageName) {
        this.colleageName = colleageName;
    }

    @Override
    public String toString() {
        return "Principal [name=" + name + ", colleageName=" + colleageName + "]";
    }

    public Principal(String name, String colleageName) {
        this.name = name;
        this.colleageName = colleageName;
    }

    @Override
    protected Principal clone() throws CloneNotSupportedException {
        return (Principal) super.clone();
    }

}
