package showcase;

public class Example8 {
    static void func1(int x, int y) {
        int z = 35;
        int w = z + 33;
        x = 398;
        if (x == 0) {
            x = w;
        } else {
            y = 5;
        }
        func2(x, y);
    }
    static void func2(int x, int y) {
    }
}
