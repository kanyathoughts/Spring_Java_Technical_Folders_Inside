import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class Longest_Descresing_order_In_array {
    public static void main(String[] args) {
        int a[] = { 12, 11, 15, 14, 13, 20, 21 };
        List<Integer> list = new ArrayList<>();
        Set<Integer> set = new LinkedHashSet<>();

        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a.length; j++) {
                if (a[i] > a[j]) {
                    set.add(a[i]);
                    set.add(a[j]);
                    System.out.println(set);
                }
            }
        }

        System.out.println(set);
    }

}
