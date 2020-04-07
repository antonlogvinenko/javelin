package javelin;

public class AndOfLongs {
    public static void main(String[] args) {
        long a = 1;
        long b = 1;
        long c = (a + a) & b;
        System.out.println(c);
    }
}
