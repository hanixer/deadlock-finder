package showcase;

public class Example7 {
    static void func1(int x, int y) {
        while (x < y) {
            if (x == 555) {
                y = 0;
                x = 0;
            } else {
                int tmp = x;
                x = y;
                y = tmp;
            }
            x = x + y;
        }
        y = x;
    }
}
