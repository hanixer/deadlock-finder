package showcase;

/**
 * Using break and continue.
 */
public class Example7 {
    public static void func1(int a, int b) {        
        int c = 0;
        while (a < b) {
            if (a == 2)
                continue;
            if (b == 2 * a)
                break;
            c = c - a;
            b = b * 1;
        }
    }
}
