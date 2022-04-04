public class ForLoop2 {
    static void func1() {
        int a = 0;
        for (int i = 0; i < 5; i++) {
            a++;
        }
        for (int j = 0; j < a; j++) {
            a = 0;
        }
    }
}