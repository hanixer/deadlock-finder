package showcase;

public class Example2 {
    public static void func1() {
        int a = 1;
        int b = 2;
        int c = a + 3 * b;
        int d = a*b + a*c;
        func2(a + b, c + d);
    }

    public static void func2(int x, int y) {
        x = x - y;
    }
}
