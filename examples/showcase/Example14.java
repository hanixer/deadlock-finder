public class Example14 {
    static int func1(int a, int b, int c, int count) {
        for (int i = 0; i < count; i++) {
            a = a + b * c;
        }
        return a;
    }
}