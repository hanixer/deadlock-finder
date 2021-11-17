package showcase;

/**
 * More complex loop
 */
public class Example2 {
    static void func1() {
        int x = 6;
        int y = 3;
        while (x < 100) {
            x = x + 1;
            while (x > 4) {
                x = x - 1;
            }
        }
    }
}