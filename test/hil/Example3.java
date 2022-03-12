package showcase;

public class Example3 {
    public static void func1(int a, int b) {
        if (a < b) {
            int c = a + b;
            a = c - b;
        } else {
            int c1 = a - b;
            a = c1 + b;
        }
        
        if (a >= b) {
            int d = a - b;
            a = d - a;
            if (d <= a) {
                a = a + a;
            }
        }
    }
}
