package showcase;

public class WhileNested {
    static void func1(int a) {
        int x = 6;
        int y = 3;
        while (x < 100) {
            x++;
            if (x == 33)
                continue;
            while (y > 0) {
                a++;
                y--;
            }
        }
    }
}