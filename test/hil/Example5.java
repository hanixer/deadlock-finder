package showcase;

public class Example5 {
    public static void func1(int a, int b) {
        int c = abs(a) - b;
        while (a + b < abs(c)) {
            c = c - a;
            b = b * 1;
        }

        {
            c = c * 2;
        }

        while (a + b < abs(c))
            c = c - a;
    }

    public static int abs(int x) {
        if (x < 0) 
            return -x;
        return x;
    }
}
