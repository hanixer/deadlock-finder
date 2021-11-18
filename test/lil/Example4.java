package showcase;

/**
 * Nested loop with break and continue
 */
public class Example4 {
    static void func1() {
        int x = 6;
        int y = 3;
        while (x < 100) {
            x = x + 1;
            if (y == 50) 
                break;
            while (x > 4) {
                if (y == 23) 
                    continue;
                x = x - 1;
            }
        }
    }
}