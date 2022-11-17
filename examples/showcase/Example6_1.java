package showcase;

/**
 * Simple loop
 */
public class Example6 {
    static void func1() {
        int x = 6;
        int y = 7;
        while (x < 100) {
            if (x == 50) {
                y = 33;
            }
            x = x + 1;
        }
        int z = x + y;
    }
}
