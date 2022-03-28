public class Assignments {
    static void func1(int a, int b, int c, int d) {
        a += b;
        d -= c;
        int x = (a *= b) + b;
        int y = (b /= c);
    }
}