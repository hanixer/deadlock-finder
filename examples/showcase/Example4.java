package showcase;

public class Example4 {
    public static void func1(int a, int b) {
        int c = abs(a) - b;
        c = c + c;
    }

    public static int abs(int x) {
        if (x < 0) 
            return -x;
        return x;
    }
}
