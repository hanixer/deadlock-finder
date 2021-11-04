package showcase;

public class Example1 {
    static void func1(String name, String surname) {
        func2(name);
    }

    static void func2(String name) {
        name = name + "surname";
    }
}